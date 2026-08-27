module TypeCheck where

import AST
import qualified Schema
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

data TypeError
  = ServerNotFound String String
  | ToolNotFound String String String
  | TooFewArgs String String Int Int
  | TooManyArgs String String Int Int
  | UnboundVar String String
  deriving (Show)

findTool :: [Schema.ToolSchema] -> String -> Maybe Schema.ToolSchema
findTool schemas name = find (\s -> Schema.toolName s == name) schemas

checkStep :: Map.Map String [Schema.ToolSchema] -> Set.Set String -> Step -> [TypeError]
checkStep schemaMap bound step = varErrors ++ callErrors
  where
    varErrors =
      [ UnboundVar (stepName step) name
      | Var name <- stepInputs step
      , not (Set.member name bound)
      ]

    callErrors =
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

-- | Checks a node against the bindings in scope, returning its errors along
-- with the bindings that are in scope after it.
checkNode :: Map.Map String [Schema.ToolSchema] -> Set.Set String -> Node
          -> ([TypeError], Set.Set String)
checkNode schemaMap bound (StepNode step) =
  (checkStep schemaMap bound step, Set.insert (bindsTo step) bound)
checkNode schemaMap bound (BranchNode branch) =
  (condErrors ++ trueErrors ++ falseErrors, Set.union trueBound falseBound)
  where
    condErrors = case branchCondition branch of
      Var name | not (Set.member name bound) -> [UnboundVar "if" name]
      _                                      -> []
    (trueErrors,  trueBound)  = checkNodes schemaMap bound (branchTrue branch)
    (falseErrors, falseBound) = checkNodes schemaMap bound (branchFalse branch)

checkNodes :: Map.Map String [Schema.ToolSchema] -> Set.Set String -> [Node]
           -> ([TypeError], Set.Set String)
checkNodes _ bound [] = ([], bound)
checkNodes schemaMap bound (n : ns) =
  let (errors, bound')      = checkNode schemaMap bound n
      (restErrors, bound'') = checkNodes schemaMap bound' ns
  in (errors ++ restErrors, bound'')

typeCheck :: Map.Map String [Schema.ToolSchema] -> Workflow -> Either [TypeError] ()
typeCheck schemaMap workflow =
  let (errors, _) = checkNodes schemaMap Set.empty (steps workflow)
  in if null errors then Right () else Left errors
