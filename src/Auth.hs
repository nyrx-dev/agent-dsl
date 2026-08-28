{-# LANGUAGE OverloadedStrings #-}
-- | Credentials for servers and endpoints that require them.
module Auth where

import AST (Auth (..), Secret (..))
import Data.String (fromString)
import Network.HTTP.Types.Header (HeaderName)
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS

-- | Headers a credential contributes to every request to one server.
type AuthHeaders = [(HeaderName, BS.ByteString)]

-- | Read the secret a declaration names. Failures name the variable, never
-- its value, so a credential cannot leak into output.
readSecret :: String -> Secret -> IO (Either String String)
readSecret label (FromEnv name) = do
  value <- lookupEnv name
  return $ case value of
    Nothing               -> Left (missing "is not set")
    Just v | null v       -> Left (missing "is set but empty")
           | otherwise    -> Right v
  where
    missing what = label ++ ": environment variable " ++ name ++ " " ++ what

resolveAuth :: String -> Auth -> IO (Either String AuthHeaders)
resolveAuth _     NoAuth                  = return (Right [])
resolveAuth label (BearerAuth secret)     =
  fmap (fmap bearerHeader) (readSecret label secret)
  where
    bearerHeader token = [("Authorization", BS.pack ("Bearer " ++ token))]
resolveAuth label (HeaderAuth name secret) =
  fmap (fmap namedHeader) (readSecret label secret)
  where
    namedHeader value = [(fromString name, BS.pack value)]
