module TypeCheck where

import AST
import qualified Schema
import Data.List (find)
import qualified Data.Map as Map

data TypeError
  = ServerNotFound String String
  | ToolNotFound String String String
  | TooFewArgs String String Int Int
  | TooManyArgs String String Int Int
  deriving (Show)

findTool :: [Schema.ToolSchema] -> String -> Maybe Schema.ToolSchema
findTool schemas name = find (\s -> Schema.toolName s == name) schemas

checkStep :: Map.Map String [Schema.ToolSchema] -> Step -> [TypeError]
checkStep schemaMap step =
  case Map.lookup (server step) schemaMap of
    Nothing -> [ServerNotFound (stepName step) (server step)]
    Just schemas ->
      case findTool schemas (toolName step) of
        Nothing -> [ToolNotFound (stepName step) (server step) (toolName step)]
        Just schema ->
          let required = length (Schema.requiredArgs schema)
              provided = length (stepInputs step)
          in if provided < required
             then [TooFewArgs (stepName step) (toolName step) required provided]
             else if provided > required
               then [TooManyArgs (stepName step) (toolName step) required provided]
               else []

checkNode :: Map.Map String [Schema.ToolSchema] -> Node -> [TypeError]
checkNode schemaMap (StepNode step) = checkStep schemaMap step
checkNode schemaMap (BranchNode branch) =
  concatMap (checkNode schemaMap) (branchTrue branch) ++
  concatMap (checkNode schemaMap) (branchFalse branch)

typeCheck :: Map.Map String [Schema.ToolSchema] -> Workflow -> Either [TypeError] ()
typeCheck schemaMap workflow =
  let errors = concatMap (checkNode schemaMap) (steps workflow)
  in if null errors then Right () else Left errors
