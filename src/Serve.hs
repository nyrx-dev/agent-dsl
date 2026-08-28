{-# LANGUAGE OverloadedStrings #-}
-- | Offer a directory of workflows to an agent as MCP tools, over stdio.
--
-- The point of the exercise: an agent gets one tool per workflow, each a
-- fixed plan that was type-checked against its servers before it ran. It
-- chooses which capability to invoke, never which calls the capability makes.
module Serve (serveDirectory) where

import AST
import Data.Aeson
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Interpreter (Context)
import Parser (workflowParser)
import Run (executeWorkflow)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Data.Text as T

data Offered = Offered
  { offeredName     :: String
  , offeredWorkflow :: Workflow
  }

protocolVersion :: String
protocolVersion = "2025-06-18"

-- | Read newline-delimited JSON-RPC from stdin and answer on stdout. Notes
-- go to stderr, which is the only reason a workflow's progress does not
-- corrupt the protocol.
serveDirectory :: FilePath -> IO ()
serveDirectory dir = do
  hSetBuffering stdout LineBuffering
  present <- doesDirectoryExist dir
  if not present
    then hPutStrLn stderr ("No such directory: " ++ dir)
    else do
      offered <- loadWorkflows dir
      hPutStrLn stderr
        ("Offering " ++ show (length offered) ++ " workflow(s) from " ++ dir)
      mapM_ (hPutStrLn stderr . ("  " ++) . offeredName) offered
      loop offered

loadWorkflows :: FilePath -> IO [Offered]
loadWorkflows dir = do
  entries <- listDirectory dir
  concat <$> mapM load (sort [e | e <- entries, takeExtension e == ".agent"])
  where
    load entry = do
      let path = dir </> entry
      contents <- readFile path
      case runParser workflowParser path contents of
        Left err -> do
          hPutStrLn stderr ("Skipping " ++ path ++ ":")
          hPutStrLn stderr (errorBundlePretty err)
          return []
        Right workflow ->
          return [Offered (takeBaseName entry) workflow]

loop :: [Offered] -> IO ()
loop offered = do
  eof <- isEOF
  if eof
    then return ()
    else do
      line <- getLine
      case decode (BL.pack line) of
        Nothing      -> loop offered
        Just request -> do
          reply <- handle offered request
          mapM_ (BL.putStrLn . encode) reply
          loop offered

-- | Answer one message. A notification has no id and gets no reply.
handle :: [Offered] -> Value -> IO (Maybe Value)
handle offered request = case (field "method" request, field "id" request) of
  (Just (String method), messageId) -> case T.unpack method of
    "initialize" -> return (fmap (ok initializeResult) messageId)
    "tools/list" -> return (fmap (ok (toolsResult offered)) messageId)
    "tools/call" -> case messageId of
      Nothing  -> return Nothing
      Just rid -> Just . ok' rid <$> callTool offered (field "params" request)
    _ -> return (fmap (\rid -> rpcError rid (-32601) ("Unknown method: " ++ T.unpack method))
                      messageId)
  _ -> return Nothing
  where
    ok result rid = ok' rid result

field :: String -> Value -> Maybe Value
field name (Object fields) = KeyMap.lookup (Key.fromString name) fields
field _    _               = Nothing

ok' :: Value -> Value -> Value
ok' messageId result = object
  [ "jsonrpc" .= ("2.0" :: String), "id" .= messageId, "result" .= result ]

rpcError :: Value -> Int -> String -> Value
rpcError messageId code message = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= messageId
  , "error"   .= object ["code" .= code, "message" .= message]
  ]

initializeResult :: Value
initializeResult = object
  [ "protocolVersion" .= protocolVersion
  , "capabilities"    .= object ["tools" .= object ["listChanged" .= False]]
  , "serverInfo"      .= object
      ["name" .= ("agent-dsl" :: String), "version" .= ("0.4.0.0" :: String)]
  ]

-- | One tool per workflow. Its arguments are the workflow's declared
-- inputs, so an agent sees a capability with a signature, not a script.
toolsResult :: [Offered] -> Value
toolsResult offered = object ["tools" .= map describe offered]
  where
    describe item = object
      [ "name"        .= offeredName item
      , "description" .= describeWorkflow item
      , "inputSchema" .= object
          [ "type"       .= ("object" :: String)
          , "properties" .= object
              [ Key.fromString (inputFieldName f) .= object
                  [ "type"        .= ("string" :: String)
                  , "description" .= fromMaybe "" (inputFieldDescription f)
                  ]
              | f <- wfInputs (offeredWorkflow item)
              ]
          , "required" .= map inputFieldName (wfInputs (offeredWorkflow item))
          ]
      ]

    describeWorkflow item =
      let workflow = offeredWorkflow item
          reported = map outputName (wfOutputs workflow)
          stepCount = length (steps workflow)
      in "Runs the fixed, pre-validated workflow " ++ workflowName workflow
         ++ " (" ++ show stepCount ++ " step(s))"
         ++ (if null reported then ""
             else ", reporting: " ++ unwords reported)
         ++ ". Every call it makes is checked against its servers' schemas "
         ++ "before any of them run."

callTool :: [Offered] -> Maybe Value -> IO Value
callTool offered params =
  case field "name" =<< params of
    Just (String name) -> case lookup (T.unpack name) named of
      Nothing   -> return (toolError ("No such workflow: " ++ T.unpack name))
      Just item -> runOffered item (argumentsOf params)
    _ -> return (toolError "tools/call needs a workflow name")
  where
    named = [(offeredName o, o) | o <- offered]

argumentsOf :: Maybe Value -> Context
argumentsOf params = case field "arguments" =<< params of
  Just (Object fields) ->
    Map.fromList [(Key.toString k, v) | (k, v) <- KeyMap.toList fields]
  _ -> Map.empty

runOffered :: Offered -> Context -> IO Value
runOffered item inputs = do
  outcome <- executeWorkflow note (offeredWorkflow item) inputs
  return $ case outcome of
    Left err          -> toolError err
    Right (result, _) -> object
      [ "content"           .= [textBlock (BL.unpack (encode result))]
      , "structuredContent" .= result
      , "isError"           .= False
      ]
  where
    note = hPutStrLn stderr

-- | A failure comes back as a tool error rather than killing the server, so
-- one broken workflow does not take the others with it.
toolError :: String -> Value
toolError message = object
  [ "content" .= [textBlock message]
  , "isError" .= True
  ]

textBlock :: String -> Value
textBlock text = object ["type" .= ("text" :: String), "text" .= text]
