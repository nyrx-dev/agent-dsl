module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map as Map

import AST (workflowName, wfServers, serverAlias, serverUrl, ServerDecl)
import Parser (workflowParser)
import qualified Schema
import TypeCheck (typeCheck)
import Interpreter (runWorkflow)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> runFile filePath
    _ -> do
      hPutStrLn stderr "Usage: agent-dsl <workflow-file>"
      exitFailure

runFile :: FilePath -> IO ()
runFile filePath = do
  contents <- readFile filePath

  case runParser workflowParser filePath contents of
    Left err -> do
      putStrLn "Parse error:"
      putStrLn (errorBundlePretty err)
      exitFailure

    Right workflow -> do
      putStrLn ("Parsed workflow: " ++ workflowName workflow)

      if null (wfServers workflow)
        then do
          hPutStrLn stderr "Workflow declares no servers. Add a 'servers:' block."
          exitFailure
        else do
          putStrLn "Fetching tool schemas..."
          (urlMap, schemaMap) <- fetchAllSchemas (wfServers workflow)

          case typeCheck schemaMap workflow of
            Left errors -> do
              putStrLn "Workflow failed type-checking:"
              mapM_ print errors
              exitFailure

            Right () -> do
              putStrLn "Type-check passed. Running workflow..."
              finalCtx <- runWorkflow urlMap schemaMap Map.empty workflow
              putStrLn "Workflow finished. Final context:"
              printContext finalCtx

fetchAllSchemas :: [ServerDecl] -> IO (Map.Map String String, Map.Map String [Schema.ToolSchema])
fetchAllSchemas decls = do
  pairs <- mapM fetchOne decls
  let urlMap = Map.fromList [(serverAlias d, serverUrl d) | d <- decls]
  return (urlMap, Map.fromList pairs)
  where
    fetchOne decl = do
      putStrLn ("  " ++ serverAlias decl ++ " -> " ++ serverUrl decl)
      schemas <- fetchSchemas (serverUrl decl)
      return (serverAlias decl, schemas)

fetchSchemas :: String -> IO [Schema.ToolSchema]
fetchSchemas url = do
  resp <- Schema.callMcp url (Schema.toolsListRequest 1)
  case Schema.decodeEvent resp :: Maybe Schema.ToolsListResult of
    Nothing -> do
      hPutStrLn stderr ("Failed to fetch or parse tool schemas from " ++ url)
      exitFailure
    Just result -> return (Schema.tools result)

printContext :: Map.Map String Value -> IO ()
printContext ctx = mapM_ printEntry (Map.toList ctx)
  where
    printEntry (name, value) = do
      putStrLn (name ++ ":")
      BLC.putStrLn (encodePretty value)
      putStrLn ""
