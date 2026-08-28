-- | Everything between a parsed workflow and its result, shared by the CLI
-- and by the server that offers workflows to an agent.
module Run where

import AST
import qualified Auth
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value)
import Data.List (intercalate)
import Interpreter (Context, collectOutputs, runWorkflow)
import qualified Schema
import TypeCheck (typeCheck)
import qualified Data.Map as Map

-- | Where progress notes go. The CLI writes them to stderr; the server
-- discards them, since its stdout carries JSON-RPC.
type Note = String -> IO ()

-- | Validate a workflow against its servers without running anything.
validateWorkflow :: Note -> Workflow -> IO (Either String ())
validateWorkflow note workflow = do
  prepared <- prepare note workflow
  return (fmap (const ()) prepared)

-- | Run a workflow with the inputs its caller supplied, reporting both what
-- it names as output and every binding it made -- the latter is what gets
-- recorded for an editor to complete from.
executeWorkflow :: Note -> Workflow -> Context -> IO (Either String (Value, Context))
executeWorkflow note workflow inputs =
  case missingInputs workflow inputs of
    missing@(_ : _) ->
      return (Left ("Missing input(s): " ++ intercalate ", " missing))
    [] -> do
      prepared <- prepare note workflow
      case prepared of
        Left err -> return (Left err)
        Right (sessions, apis, schemaMap) -> do
          note "Type-check passed. Running workflow..."
          outcome <- runWorkflow sessions apis schemaMap inputs workflow
          return $ do
            final <- outcome
            reported <- collectOutputs final (wfOutputs workflow)
            return (reported, final)

missingInputs :: Workflow -> Context -> [String]
missingInputs workflow inputs =
  [ name
  | field <- wfInputs workflow
  , let name = inputFieldName field
  , not (Map.member name inputs)
  ]

-- | Open every declared server and fetch its schemas, without checking the
-- workflow against them. An editor wants the schemas even for a file that
-- does not currently type-check -- that is exactly when it needs them.
connect :: Note -> Workflow
        -> IO (Either String ( Map.Map String Schema.Session
                             , Map.Map String (String, Auth.AuthHeaders)
                             , Map.Map String [Schema.ToolSchema] ))
connect note workflow =
  case duplicates (map serverAlias (wfServers workflow))
         ++ duplicates (map apiAlias (wfApis workflow)) of
    dupes@(_ : _) ->
      return (Left ("Duplicate alias(es): " ++ intercalate ", " dupes))
    [] -> do
      serverCreds <- mapM
        (\d -> Auth.resolveAuth ("Server " ++ serverAlias d) (serverAuth d))
        (wfServers workflow)
      apiCreds <- mapM
        (\d -> Auth.resolveAuth ("API " ++ apiAlias d) (apiAuth d))
        (wfApis workflow)
      case (sequence serverCreds, sequence apiCreds) of
        (Left err, _) -> return (Left err)
        (_, Left err) -> return (Left err)
        (Right serverHeaders, Right apiHeaders) -> do
          let apis = Map.fromList
                [ (apiAlias d, (apiBaseUrl d, headers))
                | (d, headers) <- zip (wfApis workflow) apiHeaders
                ]
          mapM_ (\d -> note ("  " ++ apiAlias d ++ " -> " ++ apiBaseUrl d))
                (wfApis workflow)
          opened <- openServers note (zip (wfServers workflow) serverHeaders)
          return (fmap (\(sessions, schemaMap) -> (sessions, apis, schemaMap)) opened)

-- | Connect, then type-check. Everything that can be checked without
-- running is checked here.
prepare :: Note -> Workflow
        -> IO (Either String ( Map.Map String Schema.Session
                             , Map.Map String (String, Auth.AuthHeaders)
                             , Map.Map String [Schema.ToolSchema] ))
prepare note workflow = do
  connected <- connect note workflow
  case connected of
    Left err -> return (Left err)
    Right (sessions, apis, schemaMap) ->
      case typeCheck schemaMap workflow of
        Left errors -> return (Left (unlines
          ("Workflow failed type-checking:" : map show errors)))
        Right () -> return (Right (sessions, apis, schemaMap))

-- | Servers are independent, and the handshake is three round trips each,
-- so opening them one after another is the slowest part of most runs.
openServers :: Note -> [(ServerDecl, Auth.AuthHeaders)]
            -> IO (Either String ( Map.Map String Schema.Session
                                 , Map.Map String [Schema.ToolSchema] ))
openServers _ [] = return (Right (Map.empty, Map.empty))
openServers note decls = do
  note "Fetching tool schemas..."
  mapM_ (\(d, _) -> note ("  " ++ serverAlias d ++ " -> " ++ serverUrl d)) decls
  entries <- mapConcurrently openOne decls
  return $ do
    opened <- sequence entries
    return ( Map.fromList [(alias, session) | (alias, session, _) <- opened]
           , Map.fromList [(alias, schemas) | (alias, _, schemas) <- opened]
           )
  where
    openOne (decl, headers) = do
      session <- Schema.openSession (serverUrl decl) headers
      case session of
        Left err -> return (Left ("Could not initialize " ++ serverUrl decl ++ ": " ++ err))
        Right opened -> do
          listed <- Schema.toolsList opened
          return $ case listed of
            Left err -> Left ("Could not list tools on " ++ serverUrl decl ++ ": " ++ err)
            Right schemas -> Right (serverAlias decl, opened, schemas)

-- | Aliases declared more than once. Map.fromList would silently keep only
-- the last URL for each, leaving the earlier one unreachable.
duplicates :: [String] -> [String]
duplicates aliases =
  [ alias | (alias, count) <- Map.toList counts, count > (1 :: Int) ]
  where
    counts = Map.fromListWith (+) [(alias, 1) | alias <- aliases]
