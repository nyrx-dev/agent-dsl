{-# LANGUAGE OverloadedStrings #-}
module Schema where

import Auth (AuthHeaders)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (listToMaybe)
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL


-- Tool schemas ---------------------------------------------------------------

data ToolSchema = ToolSchema
  { toolName     :: String
  , requiredArgs :: [String]
  -- | The schema the server declares for its structuredContent, when it
  -- declares one. Not every server does.
  , outputSchema :: Maybe Value
  } deriving (Show)

instance FromJSON ToolSchema where
  parseJSON = withObject "ToolSchema" $ \obj -> do
    name        <- obj .: "name"
    inputSchema <- obj .: "inputSchema"
    required    <- inputSchema .:? "required" .!= []
    output      <- obj .:? "outputSchema"
    return ToolSchema
      { toolName     = name
      , requiredArgs = required
      , outputSchema = output
      }

newtype ToolsListResult = ToolsListResult { tools :: [ToolSchema] }
  deriving (Show)

instance FromJSON ToolsListResult where
  parseJSON = withObject "ToolsListResult" $ \result -> do
    listed <- result .: "tools"
    return (ToolsListResult listed)


-- SSE framing ----------------------------------------------------------------

-- | Split an SSE body into its per-event data payloads, in stream order.
-- A body that is not SSE-framed (plain JSON) comes back as a single payload.
sseEvents :: BL.ByteString -> [BL.ByteString]
sseEvents bs =
  case filter (not . Prelude.null) (map dataLines (splitEvents allLines)) of
    []      -> [bs]
    chunks  -> map (BL.intercalate (BL.pack "\n")) chunks
  where
    allLines = map dropCR (BL.lines bs)

    dropCR line
      | not (BL.null line) && BL.last line == '\r' = BL.init line
      | otherwise                                  = line

    splitEvents ls = case break BL.null ls of
      (chunk, [])       -> [chunk]
      (chunk, _ : rest) -> chunk : splitEvents rest

    dataLines chunk =
      [ BL.dropWhile (== ' ') payload
      | line <- chunk
      , Just payload <- [BL.stripPrefix (BL.pack "data:") line]
      ]


-- JSON-RPC -------------------------------------------------------------------

-- | A JSON-RPC response: the request id it answers, the error it reports if
-- any, and the envelope itself. Notifications carry no id and so never parse.
data RpcEnvelope = RpcEnvelope
  { envelopeId     :: Int
  , envelopeError  :: Maybe String
  , envelopeResult :: Maybe Value
  } deriving (Show)

instance FromJSON RpcEnvelope where
  parseJSON = withObject "RpcEnvelope" $ \obj -> do
    responseId <- obj .: "id"
    err        <- obj .:? "error"
    message    <- traverse describeError err
    result     <- obj .:? "result"
    return RpcEnvelope
      { envelopeId     = responseId
      , envelopeError  = message
      , envelopeResult = result
      }
    where
      describeError :: Object -> Parser String
      describeError err = do
        message <- err .:? "message" .!= "unknown error"
        code    <- err .:? "code" .!= (0 :: Int)
        return (message ++ " (code " ++ show code ++ ")")

newtype TextBlock = TextBlock { blockText :: String }

instance FromJSON TextBlock where
  parseJSON = withObject "TextBlock" $ \obj -> TextBlock <$> obj .:? "text" .!= ""

-- | The text of a tools/call result the server flagged with isError. MCP
-- reports tool failures this way rather than as a JSON-RPC error.
toolFailure :: Value -> Maybe String
toolFailure = parseMaybe parseFailure
  where
    parseFailure = withObject "ToolFailure" $ \result -> do
      isErr <- result .:? "isError" .!= False
      if not isErr
        then fail "not a tool failure"
        else do
          blocks <- result .:? "content" .!= []
          return (unwords (map blockText blocks))

-- | Pick the response answering the given request id out of a body, skipping
-- keep-alives and notifications interleaved in the stream, and return its
-- "result" member. A server-level error or a tool-level failure comes back
-- as Left.
rpcResult :: Int -> BL.ByteString -> Either String Value
rpcResult reqId body =
  case [ env | Just env <- envelopes, envelopeId env == reqId ] of
    [] -> Left ("no JSON-RPC response with id " ++ show reqId ++ " in the reply")
    env : _ ->
      case envelopeError env of
        Just message -> Left message
        Nothing ->
          case envelopeResult env of
            Nothing     -> Left "response carried neither a result nor an error"
            Just result -> case toolFailure result of
              Just message -> Left ("tool error: " ++ message)
              Nothing      -> Right result
  where
    envelopes = map decode (sseEvents body) :: [Maybe RpcEnvelope]


-- Sessions -------------------------------------------------------------------

protocolVersion :: String
protocolVersion = "2025-06-18"

-- | A connection to one MCP server: the endpoint, the session id and protocol
-- version agreed during the handshake, and a counter so that every request
-- carries an id of its own.
data Session = Session
  { sessionUrl      :: String
  , sessionId       :: Maybe BS.ByteString
  , sessionProtocol :: Maybe BS.ByteString
  , sessionAuth     :: AuthHeaders
  , sessionNextId   :: IORef Int
  }

nextRequestId :: Session -> IO Int
nextRequestId session = atomicModifyIORef' (sessionNextId session) (\n -> (n + 1, n))

postMcp :: Session -> Value -> IO (Response BL.ByteString)
postMcp session body = do
  req0 <- parseRequest (sessionUrl session)
  let withHeader name = maybe id (\v -> setRequestHeader name [v])
      withAuth = foldr (\(name, value) f -> setRequestHeader name [value] . f)
                       id (sessionAuth session)
      request = setRequestMethod "POST"
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestHeader "Accept" ["application/json, text/event-stream"]
              $ withHeader "Mcp-Session-Id" (sessionId session)
              $ withHeader "MCP-Protocol-Version" (sessionProtocol session)
              $ withAuth
              $ setRequestBodyJSON body
              $ req0
  httpLBS request

initializeRequest :: Int -> Value
initializeRequest reqId = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= reqId
  , "method"  .= ("initialize" :: String)
  , "params"  .= object
      [ "protocolVersion" .= protocolVersion
      , "capabilities"    .= object []
      , "clientInfo"      .= object
          [ "name"    .= ("agent-dsl" :: String)
          , "version" .= ("1.0" :: String)
          ]
      ]
  ]

initializedNotification :: Value
initializedNotification = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "method"  .= ("notifications/initialized" :: String)
  , "params"  .= object []
  ]

toolsListRequest :: Int -> Value
toolsListRequest reqId = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= reqId
  , "method"  .= ("tools/list" :: String)
  , "params"  .= object []
  ]

toolsCallRequest :: Int -> String -> Value -> Value
toolsCallRequest reqId tool arguments = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= reqId
  , "method"  .= ("tools/call" :: String)
  , "params"  .= object
      [ "name"      .= tool
      , "arguments" .= arguments
      ]
  ]

-- | Run the MCP initialize handshake, which servers may require before they
-- answer anything else: initialize, adopt the session id and protocol version
-- the server replies with, then confirm with notifications/initialized.
openSession :: String -> AuthHeaders -> IO (Either String Session)
openSession url auth = do
  counter <- newIORef 1
  let handshake = Session
        { sessionUrl      = url
        , sessionId       = Nothing
        , sessionProtocol = Nothing
        , sessionAuth     = auth
        , sessionNextId   = counter
        }
  reqId    <- nextRequestId handshake
  response <- postMcp handshake (initializeRequest reqId)
  let outcome = case httpFailure response of
        Just err -> Left err
        Nothing  -> rpcResult reqId (getResponseBody response)
  case outcome of
    Left err -> return (Left err)
    Right value -> do
      let session = handshake
            { sessionId       = listToMaybe (getResponseHeader "Mcp-Session-Id" response)
            , sessionProtocol = Just (BS.pack (negotiatedVersion value))
            }
      _ <- postMcp session initializedNotification
      return (Right session)
  where
    negotiatedVersion value = case parseMaybe parseVersion value of
      Just version -> version
      Nothing      -> protocolVersion

    parseVersion = withObject "initialize result" $ \result ->
      result .:? "protocolVersion" .!= protocolVersion

-- | A non-2xx reply carries no JSON-RPC response, and its status says far
-- more than a decode failure would: 401 for a missing or wrong credential,
-- 404 for the wrong endpoint.
httpFailure :: Response BL.ByteString -> Maybe String
httpFailure response
  | status < 200 || status >= 300 =
      Just ("HTTP " ++ show status ++ ": "
            ++ take 200 (BL.unpack (getResponseBody response)))
  | otherwise = Nothing
  where
    status = getResponseStatusCode response

-- | Send one request and read the response answering it.
rpcCall :: Session -> (Int -> Value) -> IO (Either String Value)
rpcCall session buildBody = do
  reqId    <- nextRequestId session
  response <- postMcp session (buildBody reqId)
  return $ case httpFailure response of
    Just err -> Left err
    Nothing  -> rpcResult reqId (getResponseBody response)

toolsList :: Session -> IO (Either String [ToolSchema])
toolsList session = do
  outcome <- rpcCall session toolsListRequest
  return $ do
    value <- outcome
    case parseMaybe parseJSON value :: Maybe ToolsListResult of
      Nothing     -> Left "could not parse the tools/list result"
      Just result -> Right (tools result)

callTool :: Session -> String -> Value -> IO (Either String Value)
callTool session tool arguments =
  rpcCall session (\reqId -> toolsCallRequest reqId tool arguments)
