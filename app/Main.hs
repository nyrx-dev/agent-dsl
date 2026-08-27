module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Aeson (decode)
import qualified Data.Map as Map

import AST (workflowName)
import Parser (workflowParser)
import qualified Schema
import TypeCheck (typeCheck)
import Interpreter (runWorkflow)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, serverUrl] -> runFile filePath serverUrl
    _ -> do
      hPutStrLn stderr "Usage: agent-dsl <workflow-file> <mcp-server-url>"
      exitFailure

runFile :: FilePath -> String -> IO ()
runFile filePath serverUrl = do
  contents <- readFile filePath

  case runParser workflowParser filePath contents of
    Left err -> do
      putStrLn "Parse error:"
      putStrLn (errorBundlePretty err)
      exitFailure

    Right workflow -> do
      putStrLn ("Parsed workflow: " ++ workflowName workflow)
      putStrLn ("Fetching tool schemas from " ++ serverUrl ++ " ...")
      schemas <- fetchSchemas serverUrl

      case typeCheck schemas workflow of
        Left errors -> do
          putStrLn "Workflow failed type-checking:"
          mapM_ print errors
          exitFailure

        Right () -> do
          putStrLn "Type-check passed. Running workflow..."
          finalCtx <- runWorkflow serverUrl schemas Map.empty workflow
          putStrLn "Workflow finished. Final context:"
          print finalCtx

fetchSchemas :: String -> IO [Schema.ToolSchema]
fetchSchemas url = do
  resp <- Schema.callMcp url (Schema.toolsListRequest 1)
  case decode (Schema.stripSSE resp) :: Maybe Schema.ToolsListResult of
    Nothing -> do
      hPutStrLn stderr "Failed to fetch or parse tool schemas."
      exitFailure
    Just result -> return (Schema.tools result)
