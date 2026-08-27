{-# LANGUAGE OverloadedStrings #-}
-- | Plain HTTP calls, for endpoints that speak no MCP.
module Api where

import Data.Aeson
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

-- | The methods a step may use, and which of them carry a request body.
methods :: [String]
methods = ["get", "post", "put", "patch", "delete", "head"]

takesBody :: String -> Bool
takesBody method = method `elem` ["post", "put", "patch"]

-- | Call an endpoint. A non-2xx status fails the step, the way a JSON-RPC
-- error does. A JSON response body is bound as parsed JSON; anything else,
-- such as plain text or HTML, is bound as a string.
callEndpoint :: String -> String -> String -> Maybe String -> IO (Either String Value)
callEndpoint baseUrl method path body = do
  request0 <- parseRequest (baseUrl ++ path)
  let withBody = case body of
        Nothing  -> id
        Just raw -> setRequestHeader "Content-Type" ["application/json"]
                  . setRequestBodyLBS (BL.pack raw)
      request = setRequestMethod (BS.pack (map toUpper method))
              $ setRequestHeader "Accept" ["application/json"]
              -- Some APIs, GitHub's among them, refuse a request that does
              -- not name its client.
              $ setRequestHeader "User-Agent" ["agent-dsl"]
              $ withBody
              $ request0
  response <- httpLBS request
  let status  = getResponseStatusCode response
      payload = getResponseBody response
  return $
    if status < 200 || status >= 300
      then Left ("HTTP " ++ show status ++ ": " ++ take 200 (BL.unpack payload))
      else Right (fromMaybe (String (T.pack (BL.unpack payload))) (decode payload))
