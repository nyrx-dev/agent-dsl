{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import AST
import qualified Api
import Auth (AuthHeaders)
import qualified Schema
import TypeCheck (findTool)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

type Context = Map.Map String Value

-- | Follow a selector path into a value. Nothing means the path is not there.
selectPath :: [Selector] -> Value -> Maybe Value
selectPath []                 value           = Just value
selectPath (Key key   : rest) (Object fields) =
  KeyMap.lookup (Key.fromString key) fields >>= selectPath rest
selectPath (Index i   : rest) (Array items)
  | i >= 0, item : _ <- drop i (toList items) = selectPath rest item
selectPath _                  _               = Nothing

-- | The value an input stands for, or Nothing if it reaches through a
-- selector that is not there.
lookupInput :: Context -> Input -> Maybe Value
lookupInput _   (Lit s)               = Just (String (T.pack s))
lookupInput ctx (Var name selectors)  = Map.lookup name ctx >>= selectPath selectors

-- | An input in argument position. A path that is not there is an error:
-- sending null instead would surface as a confusing schema failure from the
-- server, far from the mistake.
resolveInput :: Context -> Input -> Either String Value
resolveInput ctx input = case lookupInput ctx input of
  Just value -> Right value
  Nothing    -> Left (renderInput input ++ " is not there in the bound value")

buildArgs :: [String] -> [Value] -> Value
buildArgs names values = object (zipWith (\n v -> Key.fromString n .= v) names values)

-- | How a value reads when it is spliced into a URL path: a string as its
-- own text, anything else as the JSON that would encode it.
renderForPath :: Value -> String
renderForPath (String text) = T.unpack text
renderForPath value         = BL.unpack (encode value)

runStep :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
        -> Map.Map String [Schema.ToolSchema] -> Context -> Step -> IO Context
runStep sessions apis schemaMap ctx step = case stepCall step of
  ApiCall alias method parts body ->
    case Map.lookup alias apis of
      Nothing -> error ("Unknown API alias at runtime: " ++ alias)
      Just (baseUrl, auth) ->
        case (mapM (resolveInput ctx) parts, traverse (resolveInput ctx) body) of
          (Left err, _) -> error ("Step " ++ stepName step ++ ": " ++ err)
          (_, Left err) -> error ("Step " ++ stepName step ++ ": " ++ err)
          (Right pathValues, Right bodyValue) -> do
            let path = concatMap renderForPath pathValues
            outcome <- Api.callEndpoint baseUrl auth method path
                                       (fmap renderForPath bodyValue)
            bind outcome

  ToolCall alias tool args ->
    case Map.lookup alias sessions of
      Nothing -> error ("Unknown server alias at runtime: " ++ alias)
      Just session ->
        case Map.lookup alias schemaMap of
          Nothing -> error ("No schemas fetched for server: " ++ alias)
          Just schemas ->
            case findTool schemas tool of
              Nothing -> error ("Tool not found at runtime: " ++ tool)
              Just schema ->
                case mapM (resolveInput ctx) args of
                  Left err -> error ("Step " ++ stepName step ++ ": " ++ err)
                  Right inputs -> do
                    let argsObj = buildArgs (Schema.requiredArgs schema) inputs
                    outcome <- Schema.callTool session tool argsObj
                    bind outcome
  where
    bind (Left err)     = error ("Step " ++ stepName step ++ " failed: " ++ err)
    bind (Right result) = return (Map.insert (bindsTo step) result ctx)

runNode :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
        -> Map.Map String [Schema.ToolSchema] -> Context -> Node -> IO Context
runNode sessions apis schemaMap ctx (StepNode step) =
  runStep sessions apis schemaMap ctx step
runNode sessions apis schemaMap ctx (BranchNode branch)
  | evalCondition ctx (branchCondition branch) =
      runNodes sessions apis schemaMap ctx (branchTrue branch)
  | otherwise =
      runNodes sessions apis schemaMap ctx (branchFalse branch)

-- | JSON truthiness: null, false, zero, and the empty string, array and
-- object are false. A selector that is not there counts as null, so that a
-- condition on an absent field reads the way it looks.
isTruthy :: Value -> Bool
isTruthy Null            = False
isTruthy (Bool b)        = b
isTruthy (String text)   = not (T.null text)
isTruthy (Number n)      = n /= 0
isTruthy (Array items)   = not (Prelude.null (toList items))
isTruthy (Object fields) = not (KeyMap.null fields)

-- | A condition reads a selector that is not there as null rather than
-- failing, so that it can ask about a field a tool only sometimes returns.
valueOf :: Context -> Input -> Value
valueOf ctx = fromMaybe Null . lookupInput ctx

evalCondition :: Context -> Condition -> Bool
evalCondition ctx (Truthy input) = isTruthy (valueOf ctx input)
evalCondition ctx (Compare Equals    left right) = valueOf ctx left == valueOf ctx right
evalCondition ctx (Compare NotEquals left right) = valueOf ctx left /= valueOf ctx right

runNodes :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
         -> Map.Map String [Schema.ToolSchema] -> Context -> [Node] -> IO Context
runNodes _ _ _ ctx [] = return ctx
runNodes sessions apis schemaMap ctx (n:ns) = do
  ctx' <- runNode sessions apis schemaMap ctx n
  runNodes sessions apis schemaMap ctx' ns

runWorkflow :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
            -> Map.Map String [Schema.ToolSchema] -> Context -> Workflow -> IO Context
runWorkflow sessions apis schemaMap initialCtx workflow =
  runNodes sessions apis schemaMap initialCtx (steps workflow)
