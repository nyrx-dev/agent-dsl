module Main where

import AST (Workflow, workflowName)
import Data.Aeson (Value, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Parser (workflowParser)
import Run (executeWorkflow)
import Serve (serveDirectory)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map as Map


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--serve", dir]         -> serveDirectory dir
    (filePath : assignments) -> runFile filePath assignments
    _ -> do
      hPutStrLn stderr "Usage: agent-dsl <workflow-file> [name=value ...]"
      hPutStrLn stderr "       agent-dsl --serve <directory>"
      exitFailure

runFile :: FilePath -> [String] -> IO ()
runFile filePath assignments = do
  contents <- readFile filePath
  case runParser workflowParser filePath contents of
    Left err -> do
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr (errorBundlePretty err)
      exitFailure
    Right workflow -> do
      progress ("Parsed workflow: " ++ workflowName workflow)
      inputs <- either die return (parseAssignments assignments)
      run workflow inputs

run :: Workflow -> Map.Map String Value -> IO ()
run workflow inputs = do
  outcome <- executeWorkflow progress workflow inputs
  case outcome of
    Left err     -> die err
    Right result -> BLC.putStrLn (encodePretty result)

-- | @name=value@ pairs from the command line. A value that reads as JSON is
-- taken as JSON, so a number or a list can be passed; anything else is a
-- string, which is what most inputs are.
parseAssignments :: [String] -> Either String (Map.Map String Value)
parseAssignments = fmap Map.fromList . mapM parseOne
  where
    parseOne assignment = case break (== '=') assignment of
      (name, '=' : value) | not (null name) -> Right (name, readValue value)
      _ -> Left ("Expected name=value, got: " ++ assignment)

    readValue text = case decode (BLC.pack text) of
      Just value -> value
      Nothing    -> jsonString text

    jsonString text = maybe (error "unreachable") id
      (decode (encodePretty text))

-- | Progress notes go to stderr, so that stdout carries only the workflow's
-- result and can be piped into whatever asked for it.
progress :: String -> IO ()
progress = hPutStrLn stderr
