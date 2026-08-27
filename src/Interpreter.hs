{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import AST
import qualified Schema
import TypeCheck (findTool)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import qualified Data.Text as T

type Context = Map.Map String Value

resolveInput :: Context -> Input -> Value
resolveInput _   (Lit s) = String (T.pack s)
resolveInput ctx (Var name) =
  case Map.lookup name ctx of
    Just v  -> v
    Nothing -> String (T.pack name)

buildArgs :: [String] -> [Value] -> Value
buildArgs names values = object (zipWith (\n v -> Key.fromString n .= v) names values)

runStep :: Map.Map String Schema.Session -> Map.Map String [Schema.ToolSchema] -> Context -> Step -> IO Context
runStep sessions schemaMap ctx step =
  case Map.lookup (server step) sessions of
    Nothing -> error ("Unknown server alias at runtime: " ++ server step)
    Just session ->
      case Map.lookup (server step) schemaMap of
        Nothing -> error ("No schemas fetched for server: " ++ server step)
        Just schemas ->
          case findTool schemas (toolName step) of
            Nothing -> error ("Tool not found at runtime: " ++ toolName step)
            Just schema -> do
              let inputs  = map (resolveInput ctx) (stepInputs step)
                  argsObj = buildArgs (Schema.requiredArgs schema) inputs
              outcome <- Schema.callTool session (toolName step) argsObj
              case outcome of
                Left err     -> error ("Step " ++ stepName step ++ " failed: " ++ err)
                Right result -> return (Map.insert (bindsTo step) result ctx)

runNode :: Map.Map String Schema.Session -> Map.Map String [Schema.ToolSchema] -> Context -> Node -> IO Context
runNode sessions schemaMap ctx (StepNode step) = runStep sessions schemaMap ctx step
runNode sessions schemaMap ctx (BranchNode branch) =
  let cond     = resolveInput ctx (branchCondition branch)
      isTruthy = case cond of
                   Bool False -> False
                   Null       -> False
                   _          -> True
  in if isTruthy
       then runNodes sessions schemaMap ctx (branchTrue branch)
       else runNodes sessions schemaMap ctx (branchFalse branch)

runNodes :: Map.Map String Schema.Session -> Map.Map String [Schema.ToolSchema] -> Context -> [Node] -> IO Context
runNodes _ _ ctx [] = return ctx
runNodes sessions schemaMap ctx (n:ns) = do
  ctx' <- runNode sessions schemaMap ctx n
  runNodes sessions schemaMap ctx' ns

runWorkflow :: Map.Map String Schema.Session -> Map.Map String [Schema.ToolSchema] -> Context -> Workflow -> IO Context
runWorkflow sessions schemaMap initialCtx workflow =
  runNodes sessions schemaMap initialCtx (steps workflow)
