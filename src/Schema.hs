{-# LANGUAGE OverloadedStrings #-}
module Schema where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (listToMaybe, mapMaybe)
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BL


data ToolSchema = ToolSchema
  { toolName     :: String
  , requiredArgs :: [String]
  } deriving (Show)

instance FromJSON ToolSchema where
  parseJSON = withObject "ToolSchema" $ \obj -> do
    name        <- obj .: "name"
    inputSchema <- obj .: "inputSchema"
    required    <- inputSchema .:? "required" .!= []
    return ToolSchema { toolName = name, requiredArgs = required }

initializeRequest :: Int -> Value
initializeRequest reqId = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= reqId
  , "method"  .= ("initialize" :: String)
  , "params"  .= object
      [ "protocolVersion" .= ("2025-06-18" :: String)
      , "capabilities"    .= object []
      , "clientInfo"      .= object
          [ "name"    .= ("agent-dsl" :: String)
          , "version" .= ("1.0" :: String)
          ]
      ]
  ]

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

-- | Decode the first SSE event whose payload has the expected shape. Servers
-- interleave keep-alive comments and progress notifications with the actual
-- response, so the first event is not necessarily the one we want.
decodeEvent :: FromJSON a => BL.ByteString -> Maybe a
decodeEvent bs = listToMaybe (mapMaybe decode (sseEvents bs))

-- | A JSON-RPC response, as opposed to a notification. Responses carry an
-- "id"; notifications do not.
newtype RpcResponse = RpcResponse { rpcValue :: Value }
  deriving (Show)

instance FromJSON RpcResponse where
  parseJSON = withObject "RpcResponse" $ \obj -> do
    _ <- obj .: "id" :: Parser Value
    return (RpcResponse (Object obj))

callMcp :: String -> Value -> IO BL.ByteString
callMcp url body = do
  req0 <- parseRequest url
  let request = setRequestMethod "POST"
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestHeader "Accept" ["application/json, text/event-stream"]
              $ setRequestBodyJSON body
              $ req0
  response <- httpLBS request
  return (getResponseBody response)

toolsListRequest :: Int -> Value
toolsListRequest reqId = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= reqId
  , "method"  .= ("tools/list" :: String)
  , "params"  .= object []
  ]

newtype ToolsListResult = ToolsListResult { tools :: [ToolSchema] }
  deriving (Show)

instance FromJSON ToolsListResult where
  parseJSON = withObject "ToolsListResult" $ \obj -> do
    result    <- obj .: "result"
    toolsList <- result .: "tools"
    return (ToolsListResult toolsList)
