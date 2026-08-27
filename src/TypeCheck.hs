{-# LANGUAGE OverloadedStrings #-}
module TypeCheck where

import AST
import qualified Api
import qualified Schema
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find, intercalate)
import qualified Data.Map as Map
import qualified Data.Text as T

data TypeError
  = ServerNotFound String String
  | ToolNotFound String String String
  | TooFewArgs String String Int Int
  | TooManyArgs String String Int Int
  | UnboundVar String String
  | BadPath String String String
  | ApiNotFound String String
  | UnknownMethod String String
  | BodyNotAllowed String String
  deriving (Show)

-- | What the checker knows about the world the workflow runs in.
data Env = Env
  { envSchemas :: Map.Map String [Schema.ToolSchema]
  , envApis    :: [String]
  }

-- | What each name in scope is bound to: the shape of the result the step
-- that binds it returns, where the server publishes enough to know it.
type Bindings = Map.Map String (Maybe Value)

findTool :: [Schema.ToolSchema] -> String -> Maybe Schema.ToolSchema
findTool schemas name = find (\s -> Schema.toolName s == name) schemas


-- Result shapes --------------------------------------------------------------

-- | The shape of a tools/call result: the envelope MCP specifies, with the
-- tool's own declared output schema plugged in at structuredContent. A tool
-- that declares no output schema still returns the envelope, so a path
-- through content is checkable either way.
resultSchema :: Schema.ToolSchema -> Value
resultSchema schema = object
  [ "type" .= ("object" :: String)
  , "properties" .= object
      [ "content" .= object
          [ "type" .= ("array" :: String)
          , "items" .= object
              [ "type" .= ("object" :: String)
              , "properties" .= object
                  [ "type" .= stringSchema
                  , "text" .= stringSchema
                  ]
              -- Content blocks come in several kinds -- text, image, audio,
              -- resource -- each carrying fields of its own.
              , "additionalProperties" .= True
              ]
          ]
      , "structuredContent" .= maybe anything id (Schema.outputSchema schema)
      , "isError" .= object ["type" .= ("boolean" :: String)]
      , "_meta" .= anything
      ]
  ]
  where
    stringSchema = object ["type" .= ("string" :: String)]
    anything     = object []

-- | Walk a selector path through a JSON Schema. Left describes a path that
-- cannot exist. Right covers both a path that fits and a schema too loose to
-- say either way -- an unchecked path is not an error.
checkPath :: Value -> [Selector] -> Either String ()
checkPath _      []                 = Right ()
checkPath schema (Key key : rest)   =
  case schemaType schema of
    Nothing      -> Right ()  -- untyped, or a combinator this does not read
    Just "object" -> case schemaProperties schema of
      Nothing    -> Right ()  -- an object, but of unstated shape
      Just props -> case KeyMap.lookup (Key.fromString key) props of
        Just field -> checkPath field rest
        Nothing
          | permitsExtraFields schema -> Right ()
          | otherwise -> Left ("has no field " ++ show key ++ "; it has "
                               ++ listFields props)
    Just other   -> Left ("is " ++ describeType other ++ ", not an object")
checkPath schema (Index _ : rest)   =
  case schemaType schema of
    Nothing      -> Right ()
    Just "array" -> case lookupKey "items" schema of
      Just items -> checkPath items rest
      Nothing    -> Right ()
    Just other   -> Left ("is " ++ describeType other ++ ", not an array")

lookupKey :: String -> Value -> Maybe Value
lookupKey key (Object fields) = KeyMap.lookup (Key.fromString key) fields
lookupKey _   _               = Nothing

schemaType :: Value -> Maybe String
schemaType schema = case lookupKey "type" schema of
  Just (String t) -> Just (T.unpack t)
  _               -> Nothing

schemaProperties :: Value -> Maybe Object
schemaProperties schema = case lookupKey "properties" schema of
  Just (Object props) -> Just props
  _                   -> Nothing

-- | JSON Schema lets extra fields through by default, but a schema that says
-- nothing at all about them is almost always a closed one in practice, so
-- only an explicit allowance counts here.
permitsExtraFields :: Value -> Bool
permitsExtraFields schema = case lookupKey "additionalProperties" schema of
  Nothing           -> False
  Just (Bool False) -> False
  Just _            -> True

listFields :: Object -> String
listFields props = intercalate ", " (map (show . Key.toString) (KeyMap.keys props))

describeType :: String -> String
describeType t
  | t `elem` ["array", "object", "integer"] = "an " ++ t
  | otherwise                               = "a " ++ t


-- Checking -------------------------------------------------------------------

-- | Checks one step, and reports the shape of what it binds. An API result
-- has no schema to describe it, so it binds an unknown shape and paths into
-- it are left to the runtime.
checkStep :: Env -> Bindings -> Step -> ([TypeError], Maybe Value)
checkStep env bound step = case stepCall step of
  ApiCall alias method _ body ->
    ( inputErrors
        ++ [ApiNotFound (stepName step) alias | alias `notElem` envApis env]
        ++ [UnknownMethod (stepName step) method | method `notElem` Api.methods]
        ++ [ BodyNotAllowed (stepName step) method
           | Just _ <- [body], method `elem` Api.methods, not (Api.takesBody method) ]
    , Nothing
    )
  ToolCall alias tool args ->
    case Map.lookup alias (envSchemas env) of
      Nothing -> (inputErrors ++ [ServerNotFound (stepName step) alias], Nothing)
      Just schemas ->
        case findTool schemas tool of
          Nothing -> (inputErrors ++ [ToolNotFound (stepName step) alias tool], Nothing)
          Just schema ->
            let required = length (Schema.requiredArgs schema)
                provided = length args
                arity
                  | provided < required =
                      [TooFewArgs (stepName step) tool required provided]
                  | provided > required =
                      [TooManyArgs (stepName step) tool required provided]
                  | otherwise = []
            in (inputErrors ++ arity, Just (resultSchema schema))
  where
    inputErrors = concatMap checkInput (callInputs (stepCall step))

    checkInput (Lit _) = []
    checkInput input@(Var name selectors) = case Map.lookup name bound of
      Nothing           -> [UnboundVar (stepName step) name]
      Just Nothing      -> []
      Just (Just shape) -> case checkPath shape selectors of
        Right ()    -> []
        Left reason -> [BadPath (stepName step) (renderInput input) reason]

conditionInputs :: Condition -> [Input]
conditionInputs (Truthy input)         = [input]
conditionInputs (Compare _ left right) = [left, right]

-- | Checks a node against the bindings in scope, returning its errors along
-- with the bindings that are in scope after it.
checkNode :: Env -> Bindings -> Node -> ([TypeError], Bindings)
checkNode env bound (StepNode step) =
  let (errors, shape) = checkStep env bound step
  in (errors, Map.insert (bindsTo step) shape bound)
checkNode env bound (BranchNode branch) =
  ( condErrors ++ trueErrors ++ falseErrors
  , Map.unionWith mergeShapes trueBound falseBound
  )
  where
    -- A condition reads a path that is not there as null rather than
    -- failing, so only the name itself is checked here.
    condErrors =
      [ UnboundVar "if" name
      | Var name _ <- conditionInputs (branchCondition branch)
      , not (Map.member name bound)
      ]
    (trueErrors,  trueBound)  = checkNodes env bound (branchTrue branch)
    (falseErrors, falseBound) = checkNodes env bound (branchFalse branch)

    -- A name bound to a different shape in each arm has no one shape after.
    mergeShapes left right
      | left == right = left
      | otherwise     = Nothing

checkNodes :: Env -> Bindings -> [Node] -> ([TypeError], Bindings)
checkNodes _ bound [] = ([], bound)
checkNodes env bound (n : ns) =
  let (errors, bound')      = checkNode env bound n
      (restErrors, bound'') = checkNodes env bound' ns
  in (errors ++ restErrors, bound'')

typeCheck :: Map.Map String [Schema.ToolSchema] -> Workflow -> Either [TypeError] ()
typeCheck schemaMap workflow =
  let env = Env
        { envSchemas = schemaMap
        , envApis    = map apiAlias (wfApis workflow)
        }
      (errors, _) = checkNodes env Map.empty (steps workflow)
  in if null errors then Right () else Left errors
