module Main where

import Control.Concurrent.Async (mapConcurrently)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (intercalate)
import qualified Data.Map as Map

import qualified AST
import AST (workflowName, wfServers, wfApis, wfOutputs, serverAlias, serverUrl,
            serverAuth, apiAlias, apiBaseUrl, apiAuth, ServerDecl)
import qualified Auth
import Parser (workflowParser)
import qualified Schema
import TypeCheck (typeCheck)
import Interpreter (collectOutputs, runWorkflow)


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
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr (errorBundlePretty err)
      exitFailure

    Right workflow -> do
      progress ("Parsed workflow: " ++ workflowName workflow)

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

          -- Credentials are read before anything is contacted, so a missing
          -- one fails immediately rather than as a 401 several steps in.
          serverHeaders <- mapM
            (\d -> resolveOrDie ("Server " ++ serverAlias d) (serverAuth d))
            (wfServers workflow)
          apiHeaders <- mapM
            (\d -> resolveOrDie ("API " ++ apiAlias d) (apiAuth d))
            (wfApis workflow)

          let apis = Map.fromList
                [ (apiAlias d, (apiBaseUrl d, headers))
                | (d, headers) <- zip (wfApis workflow) apiHeaders
                ]
          mapM_ (\d -> progress ("  " ++ apiAlias d ++ " -> " ++ apiBaseUrl d))
                (wfApis workflow)

          (sessions, schemaMap) <-
            if null (wfServers workflow)
              then return (Map.empty, Map.empty)
              else do
                progress "Fetching tool schemas..."
                fetchAllSchemas (zip (wfServers workflow) serverHeaders)

          case typeCheck schemaMap workflow of
            Left errors -> do
              hPutStrLn stderr "Workflow failed type-checking:"
              mapM_ (hPutStrLn stderr . show) errors
              exitFailure

            Right () -> do
              progress "Type-check passed. Running workflow..."
              finalCtx <- runWorkflow sessions apis schemaMap Map.empty workflow
              case collectOutputs finalCtx (wfOutputs workflow) of
                Left err     -> die ("Output: " ++ err)
                Right result -> BLC.putStrLn (encodePretty result)

-- | Aliases declared more than once. Map.fromList would silently keep only
-- the last URL for each, leaving the earlier one unreachable.
duplicates :: [String] -> [String]
duplicates aliases =
  [ alias | (alias, count) <- Map.toList counts, count > (1 :: Int) ]
  where
    counts = Map.fromListWith (+) [(alias, 1) | alias <- aliases]

resolveOrDie :: String -> AST.Auth -> IO Auth.AuthHeaders
resolveOrDie label auth = do
  resolved <- Auth.resolveAuth label auth
  either die return resolved

-- | Open a session against every declared server and fetch its tool schemas.
fetchAllSchemas :: [(ServerDecl, Auth.AuthHeaders)]
                -> IO (Map.Map String Schema.Session, Map.Map String [Schema.ToolSchema])
fetchAllSchemas decls = do
  mapM_ (\(decl, _) -> progress ("  " ++ serverAlias decl ++ " -> " ++ serverUrl decl)) decls
  -- Servers are independent, and the handshake is three round trips each, so
  -- opening them one after another is the slowest part of most runs.
  entries <- mapConcurrently openOne decls
  return ( Map.fromList [(alias, session) | (alias, session, _) <- entries]
         , Map.fromList [(alias, schemas) | (alias, _, schemas) <- entries]
         )
  where
    openOne (decl, headers) = do
      opened <- Schema.openSession (serverUrl decl) headers
      case opened of
        Left err -> die ("Could not initialize " ++ serverUrl decl ++ ": " ++ err)
        Right session -> do
          listed <- Schema.toolsList session
          case listed of
            Left err      -> die ("Could not list tools on " ++ serverUrl decl ++ ": " ++ err)
            Right schemas -> return (serverAlias decl, session, schemas)

-- | Progress notes go to stderr, so that stdout carries only the workflow's
-- result and can be piped into whatever asked for it.
progress :: String -> IO ()
progress = hPutStrLn stderr
