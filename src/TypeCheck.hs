{-# LANGUAGE OverloadedStrings #-}
module TypeCheck where

import AST
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
  deriving (Show)

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

-- | Checks one step, and reports the shape of what it binds.
checkStep :: Map.Map String [Schema.ToolSchema] -> Bindings -> Step
          -> ([TypeError], Maybe Value)
checkStep schemaMap bound step =
  case Map.lookup (server step) schemaMap of
    Nothing ->
      (inputErrors ++ [ServerNotFound (stepName step) (server step)], Nothing)
    Just schemas ->
      case findTool schemas (toolName step) of
        Nothing ->
          ( inputErrors ++ [ToolNotFound (stepName step) (server step) (toolName step)]
          , Nothing
          )
        Just schema ->
          let required = length (Schema.requiredArgs schema)
              provided = length (stepInputs step)
              arity
                | provided < required =
                    [TooFewArgs (stepName step) (toolName step) required provided]
                | provided > required =
                    [TooManyArgs (stepName step) (toolName step) required provided]
                | otherwise = []
          in (inputErrors ++ arity, Just (resultSchema schema))
  where
    inputErrors = concatMap checkInput (stepInputs step)

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
checkNode :: Map.Map String [Schema.ToolSchema] -> Bindings -> Node
          -> ([TypeError], Bindings)
checkNode schemaMap bound (StepNode step) =
  let (errors, shape) = checkStep schemaMap bound step
  in (errors, Map.insert (bindsTo step) shape bound)
checkNode schemaMap bound (BranchNode branch) =
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
    (trueErrors,  trueBound)  = checkNodes schemaMap bound (branchTrue branch)
    (falseErrors, falseBound) = checkNodes schemaMap bound (branchFalse branch)

    -- A name bound to a different shape in each arm has no one shape after.
    mergeShapes left right
      | left == right = left
      | otherwise     = Nothing

checkNodes :: Map.Map String [Schema.ToolSchema] -> Bindings -> [Node]
           -> ([TypeError], Bindings)
checkNodes _ bound [] = ([], bound)
checkNodes schemaMap bound (n : ns) =
  let (errors, bound')      = checkNode schemaMap bound n
      (restErrors, bound'') = checkNodes schemaMap bound' ns
  in (errors ++ restErrors, bound'')

typeCheck :: Map.Map String [Schema.ToolSchema] -> Workflow -> Either [TypeError] ()
typeCheck schemaMap workflow =
  let (errors, _) = checkNodes schemaMap Map.empty (steps workflow)
  in if null errors then Right () else Left errors
