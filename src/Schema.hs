{-# LANGUAGE OverloadedStrings #-}
module Schema where

import Data.Aeson
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

fetchToolSchema :: String -> IO (Maybe ToolSchema)
fetchToolSchema url = do
  request  <- parseRequest url
  response <- httpLBS request
  let body = getResponseBody response
  return (decode body)

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

stripSSE :: BL.ByteString -> BL.ByteString
stripSSE bs =
  case BL.stripPrefix (BL.pack "data: ") (BL.dropWhile (/= 'd') bs) of
    Just rest -> rest
    Nothing   -> bs

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
