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
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (intercalate, isInfixOf)
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
resolveInput _   (Lit s) = Right (String (T.pack s))
resolveInput ctx input@(Var name selectors) = case Map.lookup name ctx of
  Nothing -> Left (name ++ " is not bound")
  Just value -> case selectPath selectors value of
    Just found -> Right found
    Nothing    -> Left (renderInput input ++ " " ++ explainPath value selectors)

-- | Why a path did not fit, pointed at the selector that failed rather than
-- at the whole path, and saying what was there instead.
explainPath :: Value -> [Selector] -> String
explainPath _     []                = "is not there"
explainPath value (Key key : rest)  = case value of
  Object fields -> case KeyMap.lookup (Key.fromString key) fields of
    Just found -> explainPath found rest
    Nothing    -> "has no field " ++ show key
                  ++ suggest key (map Key.toString (KeyMap.keys fields))
  _ -> "is " ++ describeValue value ++ ", not an object"
explainPath value (Index i : rest)  = case value of
  Array items
    | item : _ <- drop i (toList items) -> explainPath item rest
    | otherwise -> "has no item " ++ show i ++ ", holding "
                   ++ show (length (toList items)) ++ " of them"
  _ -> "is " ++ describeValue value ++ ", not an array"

-- | A near miss is far more use than the whole list of fields, and a
-- mistyped path is usually a near miss.
suggest :: String -> [String] -> String
suggest wanted available = case near of
  []      -> "; it has " ++ summarise available
  matches -> "; did you mean " ++ intercalate " or " (map show (take 3 matches)) ++ "?"
  where
    near =
      [ key | key <- available
      , key /= wanted
      , lower wanted `isInfixOf` lower key || lower key `isInfixOf` lower wanted
      ]
    lower = map toLower

summarise :: [String] -> String
summarise keys
  | extra <= 0 = intercalate ", " (map show keys)
  | otherwise  = intercalate ", " (map show (take shown keys))
                 ++ ", and " ++ show extra ++ " more"
  where
    shown = 12
    extra = length keys - shown

describeValue :: Value -> String
describeValue Null       = "null"
describeValue (Bool _)   = "a boolean"
describeValue (Number _) = "a number"
describeValue (String _) = "a string"
describeValue (Array _)  = "an array"
describeValue (Object _) = "an object"

buildArgs :: [String] -> [Value] -> Value
buildArgs names values = object (zipWith (\n v -> Key.fromString n .= v) names values)

-- | How a value reads when it is spliced into a URL path: a string as its
-- own text, anything else as the JSON that would encode it.
renderForPath :: Value -> String
renderForPath (String text) = T.unpack text
renderForPath value         = BL.unpack (encode value)

-- | Running a workflow reports a failure rather than aborting, so that a
-- caller -- an agent asking for one workflow among many, say -- keeps going.
runStep :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
        -> Map.Map String [Schema.ToolSchema] -> Context -> Step
        -> IO (Either String Context)
runStep sessions apis schemaMap ctx step = case stepCall step of
  ApiCall alias method parts body ->
    case Map.lookup alias apis of
      Nothing -> failed ("unknown API alias " ++ alias)
      Just (baseUrl, auth) ->
        case (mapM (resolveInput ctx) parts, traverse (resolveInput ctx) body) of
          (Left err, _) -> failed err
          (_, Left err) -> failed err
          (Right pathValues, Right bodyValue) -> do
            let path = concatMap renderForPath pathValues
            outcome <- Api.callEndpoint baseUrl auth method path
                                       (fmap renderForPath bodyValue)
            bind outcome

  ToolCall alias tool args ->
    case Map.lookup alias sessions of
      Nothing -> failed ("unknown server alias " ++ alias)
      Just session ->
        case Map.lookup alias schemaMap of
          Nothing -> failed ("no schemas fetched for server " ++ alias)
          Just schemas ->
            case findTool schemas tool of
              Nothing -> failed ("tool not found: " ++ tool)
              Just schema ->
                case mapM (resolveInput ctx) args of
                  Left err -> failed err
                  Right inputs -> do
                    let argsObj = buildArgs (Schema.requiredArgs schema) inputs
                    outcome <- Schema.callTool session tool argsObj
                    bind outcome
  where
    failed reason = return (Left ("Step " ++ stepName step ++ ": " ++ reason))
    bind (Left err)     = failed err
    bind (Right result) = return (Right (Map.insert (bindsTo step) result ctx))

runNode :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
        -> Map.Map String [Schema.ToolSchema] -> Context -> Node
        -> IO (Either String Context)
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
         -> Map.Map String [Schema.ToolSchema] -> Context -> [Node]
         -> IO (Either String Context)
runNodes _ _ _ ctx [] = return (Right ctx)
runNodes sessions apis schemaMap ctx (n:ns) = do
  outcome <- runNode sessions apis schemaMap ctx n
  case outcome of
    Left err   -> return (Left err)
    Right ctx' -> runNodes sessions apis schemaMap ctx' ns

runWorkflow :: Map.Map String Schema.Session -> Map.Map String (String, AuthHeaders)
            -> Map.Map String [Schema.ToolSchema] -> Context -> Workflow
            -> IO (Either String Context)
runWorkflow sessions apis schemaMap initialCtx workflow =
  runNodes sessions apis schemaMap initialCtx (steps workflow)

-- | The values a workflow reports, as a JSON object. A workflow that names
-- none reports everything it bound.
collectOutputs :: Context -> [OutputField] -> Either String Value
collectOutputs ctx [] =
  Right (object [Key.fromString name .= value | (name, value) <- Map.toList ctx])
collectOutputs ctx fields = do
  named <- mapM resolveField fields
  return (object named)
  where
    resolveField field = case resolveInput ctx (outputValue field) of
      Right value -> Right (Key.fromString (outputName field) .= value)
      Left err    -> Left ("Output " ++ outputName field ++ ": " ++ err)
