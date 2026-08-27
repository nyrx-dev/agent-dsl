module TypeCheck where

import AST
import qualified Schema
import Data.List (find)

data TypeError
  = ToolNotFound String String        -- step name, tool name
  | TooFewArgs String String Int Int  -- step name, tool name, required count, provided count
  deriving (Show)

findTool :: [Schema.ToolSchema] -> String -> Maybe Schema.ToolSchema
findTool schemas name = find (\s -> Schema.toolName s == name) schemas

checkStep :: [Schema.ToolSchema] -> Step -> [TypeError]
checkStep schemas step =
  case findTool schemas (toolName step) of
    Nothing -> [ToolNotFound (stepName step) (toolName step)]
    Just schema ->
      let required = length (Schema.requiredArgs schema)
          provided = length (stepInputs step)
      in if provided < required
         then [TooFewArgs (stepName step) (toolName step) required provided]
         else []

checkNode :: [Schema.ToolSchema] -> Node -> [TypeError]
checkNode schemas (StepNode step) = checkStep schemas step
checkNode schemas (BranchNode branch) =
  concatMap (checkNode schemas) (branchTrue branch) ++
  concatMap (checkNode schemas) (branchFalse branch)

typeCheck :: [Schema.ToolSchema] -> Workflow -> Either [TypeError] ()
typeCheck schemas workflow =
  let errors = concatMap (checkNode schemas) (steps workflow)
  in if null errors then Right () else Left errors
