{-# LANGUAGE OverloadedStrings #-}
-- | What a workflow's bindings actually looked like the last time it ran.
--
-- An HTTP endpoint publishes no schema, so nothing can say ahead of time
-- what @latest.@ offers. Recording the shape of each binding after a run
-- gives an editor something real to complete from -- what the API returned,
-- not what it documents. The cache is only ever used for suggestions, never
-- to type-check a workflow, so a stale entry cannot turn into a false error.
module Shapes where

import Data.Aeson
import Data.Foldable (toList)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map

-- | Shapes are recorded as JSON Schema, the same language the MCP tool
-- schemas speak, so that one path-walker serves both.
inferSchema :: Value -> Value
inferSchema (Object fields) = object
  [ "type"       .= ("object" :: String)
  , "properties" .= Object (KeyMap.map inferSchema fields)
  ]
inferSchema (Array items) = object
  ([ "type" .= ("array" :: String) ] ++
   case toList items of
     first : _ -> ["items" .= inferSchema first]
     []        -> [])
inferSchema (String _) = object ["type" .= ("string" :: String)]
inferSchema (Number _) = object ["type" .= ("number" :: String)]
inferSchema (Bool _)   = object ["type" .= ("boolean" :: String)]
inferSchema Null       = object []

cacheFile :: FilePath -> FilePath
cacheFile workflowPath = takeDirectory workflowPath </> ".agent-dsl-shapes.json"

-- | Record the shape of every binding a run produced.
recordShapes :: FilePath -> Map.Map String Value -> IO ()
recordShapes workflowPath bindings = do
  let path = cacheFile workflowPath
  createDirectoryIfMissing True (takeDirectory path)
  existing <- readCache path
  let entry = Object (KeyMap.fromList
        [(Key.fromString name, inferSchema value) | (name, value) <- Map.toList bindings])
      updated = KeyMap.insert (Key.fromString (takeFileName workflowPath)) entry existing
  BL.writeFile path (encode (Object updated))

-- | The shapes recorded for one workflow, by binding name.
loadShapes :: FilePath -> IO (Map.Map String Value)
loadShapes workflowPath = do
  cached <- readCache (cacheFile workflowPath)
  return $ case KeyMap.lookup (Key.fromString (takeFileName workflowPath)) cached of
    Just (Object entry) ->
      Map.fromList [(Key.toString k, v) | (k, v) <- KeyMap.toList entry]
    _ -> Map.empty

readCache :: FilePath -> IO Object
readCache path = do
  present <- doesFileExist path
  if not present
    then return KeyMap.empty
    else do
      contents <- BL.readFile path
      return $ case decode contents of
        Just (Object fields) -> fields
        _                    -> KeyMap.empty
