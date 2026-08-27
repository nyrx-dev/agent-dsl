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

runStep :: Map.Map String String -> Map.Map String [Schema.ToolSchema] -> Context -> Step -> IO Context
runStep urlMap schemaMap ctx step =
  case Map.lookup (server step) urlMap of
    Nothing -> error ("Unknown server alias at runtime: " ++ server step)
    Just url ->
      case Map.lookup (server step) schemaMap of
        Nothing -> error ("No schemas fetched for server: " ++ server step)
        Just schemas ->
          case findTool schemas (toolName step) of
            Nothing -> error ("Tool not found at runtime: " ++ toolName step)
            Just schema -> do
              let inputs  = map (resolveInput ctx) (stepInputs step)
                  argsObj = buildArgs (Schema.requiredArgs schema) inputs
                  body = object
                    [ "jsonrpc" .= ("2.0" :: String)
                    , "id"      .= (1 :: Int)
                    , "method"  .= ("tools/call" :: String)
                    , "params"  .= object
                        [ "name"      .= toolName step
                        , "arguments" .= argsObj
                        ]
                    ]
              respBytes <- Schema.callMcp url body
              case Schema.decodeEvent respBytes :: Maybe Schema.RpcResponse of
                Nothing   -> error ("Couldn't parse response for step " ++ stepName step)
                Just resp -> return (Map.insert (bindsTo step) (Schema.rpcValue resp) ctx)

runNode :: Map.Map String String -> Map.Map String [Schema.ToolSchema] -> Context -> Node -> IO Context
runNode urlMap schemaMap ctx (StepNode step) = runStep urlMap schemaMap ctx step
runNode urlMap schemaMap ctx (BranchNode branch) =
  let cond     = resolveInput ctx (branchCondition branch)
      isTruthy = case cond of
                   Bool False -> False
                   Null       -> False
                   _          -> True
  in if isTruthy
       then runNodes urlMap schemaMap ctx (branchTrue branch)
       else runNodes urlMap schemaMap ctx (branchFalse branch)

runNodes :: Map.Map String String -> Map.Map String [Schema.ToolSchema] -> Context -> [Node] -> IO Context
runNodes _ _ ctx [] = return ctx
runNodes urlMap schemaMap ctx (n:ns) = do
  ctx' <- runNode urlMap schemaMap ctx n
  runNodes urlMap schemaMap ctx' ns

runWorkflow :: Map.Map String String -> Map.Map String [Schema.ToolSchema] -> Context -> Workflow -> IO Context
runWorkflow urlMap schemaMap initialCtx workflow =
  runNodes urlMap schemaMap initialCtx (steps workflow)
