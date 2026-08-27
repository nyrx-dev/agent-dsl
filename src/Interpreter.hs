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

runStep :: String -> [Schema.ToolSchema] -> Context -> Step -> IO Context
runStep url schemas ctx step =
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
      case decode (Schema.stripSSE respBytes) :: Maybe Value of
        Nothing     -> error ("Couldn't parse response for step " ++ stepName step)
        Just result -> return (Map.insert (bindsTo step) result ctx)

runNode :: String -> [Schema.ToolSchema] -> Context -> Node -> IO Context
runNode url schemas ctx (StepNode step) = runStep url schemas ctx step
runNode url schemas ctx (BranchNode branch) =
  let cond     = resolveInput ctx (branchCondition branch)
      isTruthy = case cond of
                   Bool False -> False
                   Null       -> False
                   _          -> True
  in if isTruthy
       then runNodes url schemas ctx (branchTrue branch)
       else runNodes url schemas ctx (branchFalse branch)

runNodes :: String -> [Schema.ToolSchema] -> Context -> [Node] -> IO Context
runNodes _   _       ctx []     = return ctx
runNodes url schemas ctx (n:ns) = do
  ctx' <- runNode url schemas ctx n
  runNodes url schemas ctx' ns

runWorkflow :: String -> [Schema.ToolSchema] -> Context -> Workflow -> IO Context
runWorkflow url schemas initialCtx workflow =
  runNodes url schemas initialCtx (steps workflow)
