module Main where

import Control.Concurrent.Async (mapConcurrently)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (intercalate)
import qualified Data.Map as Map

import AST (workflowName, wfServers, wfApis, serverAlias, serverUrl,
            apiAlias, apiBaseUrl, ServerDecl)
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

      if null (wfServers workflow) && null (wfApis workflow)
        then do
          hPutStrLn stderr
            "Workflow declares nothing to call. Add a 'servers:' or 'apis:' block."
          exitFailure
        else do
          case duplicates (map serverAlias (wfServers workflow)) of
            []    -> return ()
            dupes -> die ("Duplicate server alias(es): " ++ intercalate ", " dupes)
          case duplicates (map apiAlias (wfApis workflow)) of
            []    -> return ()
            dupes -> die ("Duplicate API alias(es): " ++ intercalate ", " dupes)

          let apis = Map.fromList
                [(apiAlias d, apiBaseUrl d) | d <- wfApis workflow]
          mapM_ (\d -> putStrLn ("  " ++ apiAlias d ++ " -> " ++ apiBaseUrl d))
                (wfApis workflow)

          (sessions, schemaMap) <-
            if null (wfServers workflow)
              then return (Map.empty, Map.empty)
              else do
                putStrLn "Fetching tool schemas..."
                fetchAllSchemas (wfServers workflow)

          case typeCheck schemaMap workflow of
            Left errors -> do
              putStrLn "Workflow failed type-checking:"
              mapM_ print errors
              exitFailure

            Right () -> do
              putStrLn "Type-check passed. Running workflow..."
              finalCtx <- runWorkflow sessions apis schemaMap Map.empty workflow
              putStrLn "Workflow finished. Final context:"
              printContext finalCtx

-- | Aliases declared more than once. Map.fromList would silently keep only
-- the last URL for each, leaving the earlier one unreachable.
duplicates :: [String] -> [String]
duplicates aliases =
  [ alias | (alias, count) <- Map.toList counts, count > (1 :: Int) ]
  where
    counts = Map.fromListWith (+) [(alias, 1) | alias <- aliases]

-- | Open a session against every declared server and fetch its tool schemas.
fetchAllSchemas :: [ServerDecl]
                -> IO (Map.Map String Schema.Session, Map.Map String [Schema.ToolSchema])
fetchAllSchemas decls = do
  mapM_ (\decl -> putStrLn ("  " ++ serverAlias decl ++ " -> " ++ serverUrl decl)) decls
  -- Servers are independent, and the handshake is three round trips each, so
  -- opening them one after another is the slowest part of most runs.
  entries <- mapConcurrently openOne decls
  return ( Map.fromList [(alias, session) | (alias, session, _) <- entries]
         , Map.fromList [(alias, schemas) | (alias, _, schemas) <- entries]
         )
  where
    openOne decl = do
      opened <- Schema.openSession (serverUrl decl)
      case opened of
        Left err -> die ("Could not initialize " ++ serverUrl decl ++ ": " ++ err)
        Right session -> do
          listed <- Schema.toolsList session
          case listed of
            Left err      -> die ("Could not list tools on " ++ serverUrl decl ++ ": " ++ err)
            Right schemas -> return (serverAlias decl, session, schemas)

printContext :: Map.Map String Value -> IO ()
printContext ctx = mapM_ printEntry (Map.toList ctx)
  where
    printEntry (name, value) = do
      putStrLn (name ++ ":")
      BLC.putStrLn (encodePretty value)
      putStrLn ""
