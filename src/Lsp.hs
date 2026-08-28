{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A language server for .agent files.
--
-- Two things an editor can offer that the command line cannot: the
-- type-checker's findings as you type, and completion of a path into an
-- earlier step's result. The first reuses the checker as it stands. The
-- second draws on tool schemas where a server publishes them, and on the
-- shapes of the last run where nothing does.
module Lsp
  ( runLanguageServer
  -- exposed for tests
  , fieldsAt
  , pathBeforeCursor
  , bindingSource
  , declaredAliases
  ) where

import AST
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Char (isAlphaNum, isDigit)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Parser (workflowParser)
import Run (connect)
import qualified Schema
import qualified Shapes
import System.IO
import Text.Megaparsec (errorBundlePretty, runParser)
import TypeCheck
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Data.Text as T

-- | Open documents, and the tool schemas already fetched for them. Schemas
-- are fetched when a file is opened or saved, never per keystroke.
data ServerState = ServerState
  { openDocuments :: Map.Map String String
  , knownSchemas  :: Map.Map String (Map.Map String [Schema.ToolSchema])
  }

runLanguageServer :: IO ()
runLanguageServer = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  hSetBinaryMode stdout True
  state <- newIORef (ServerState Map.empty Map.empty)
  loop state

loop :: IORef ServerState -> IO ()
loop state = do
  incoming <- readMessage
  case incoming of
    Nothing      -> return ()
    Just request -> do
      keepGoing <- dispatch state request
      if keepGoing then loop state else return ()

-- LSP framing ----------------------------------------------------------------

-- | LSP frames each message with a Content-Length header, unlike the
-- newline-delimited JSON the MCP server speaks.
readMessage :: IO (Maybe Value)
readMessage = do
  len <- readHeaders (0 :: Int)
  case len of
    Nothing     -> return Nothing
    Just 0      -> return Nothing
    Just length' -> do
      body <- readBytes length'
      return (decode (BL.pack body))
  where
    readHeaders found = do
      eof <- isEOF
      if eof then return Nothing else do
        line <- getLine
        let stripped = filter (/= '\r') line
        if null stripped
          then return (Just found)
          else case stripPrefixCI "content-length:" stripped of
            Just rest -> readHeaders (read (trim rest))
            Nothing   -> readHeaders found

    readBytes n = mapM (const getChar) [1 .. n]

    stripPrefixCI prefix text
      | map lower prefix `isPrefixOf` map lower text = Just (drop (length prefix) text)
      | otherwise = Nothing
    lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    trim = dropWhile (== ' ')

send :: Value -> IO ()
send payload = do
  let body = encode payload
  BL.hPutStr stdout (BL.pack ("Content-Length: " ++ show (BL.length body) ++ "\r\n\r\n"))
  BL.hPutStr stdout body
  hFlush stdout

respond :: Value -> Value -> IO ()
respond messageId result' =
  send (object ["jsonrpc" .= ("2.0" :: String), "id" .= messageId, "result" .= result'])

notify :: String -> Value -> IO ()
notify method params' =
  send (object ["jsonrpc" .= ("2.0" :: String), "method" .= method, "params" .= params'])

-- Dispatch -------------------------------------------------------------------

dispatch :: IORef ServerState -> Value -> IO Bool
dispatch state request = case textField "method" request of
  Just "initialize" -> do
    maybe (return ()) (`respond` initializeResult) (field "id" request)
    return True
  Just "shutdown" -> do
    maybe (return ()) (`respond` Null) (field "id" request)
    return True
  Just "exit" -> return False
  Just "textDocument/didOpen" -> do
    withDocument state request True
    return True
  Just "textDocument/didSave" -> do
    withDocument state request True
    return True
  Just "textDocument/didChange" -> do
    withDocument state request False
    return True
  Just "textDocument/completion" -> do
    case field "id" request of
      Nothing -> return ()
      Just messageId -> do
        items <- completionsFor state request
        respond messageId (object ["isIncomplete" .= False, "items" .= items])
    return True
  _ -> return True

initializeResult :: Value
initializeResult = object
  [ "capabilities" .= object
      [ "textDocumentSync" .= (1 :: Int)  -- full text on every change
      , "completionProvider" .= object
          ["triggerCharacters" .= ([".", ":"] :: [String])]
      ]
  , "serverInfo" .= object
      ["name" .= ("agent-dsl" :: String), "version" .= ("0.5.0.0" :: String)]
  ]

-- Documents ------------------------------------------------------------------

field :: String -> Value -> Maybe Value
field name (Object fields) = KeyMap.lookup (Key.fromString name) fields
field _    _               = Nothing

textField :: String -> Value -> Maybe String
textField name value = case field name value of
  Just (String text) -> Just (T.unpack text)
  _                  -> Nothing

intField :: String -> Value -> Maybe Int
intField name value = case field name value of
  Just (Number n) -> Just (truncate n)
  _               -> Nothing

documentUri :: Value -> Maybe String
documentUri request =
  textField "uri" =<< field "textDocument" =<< field "params" request

-- | The whole document, since the server asks for full syncs.
documentText :: Value -> Maybe String
documentText request = case field "params" request of
  Nothing -> Nothing
  Just params -> case field "textDocument" params >>= field "text" of
    Just (String text) -> Just (T.unpack text)
    _ -> case field "contentChanges" params of
      Just (Array changes) -> case reverse (foldr (:) [] changes) of
        change : _ -> textField "text" change
        []         -> Nothing
      _ -> Nothing

pathOfUri :: String -> FilePath
pathOfUri uri = unescape (fromMaybe uri (stripPrefix' "file://" uri))
  where
    stripPrefix' p t = if p `isPrefixOf` t then Just (drop (length p) t) else Nothing
    unescape ('%' : a : b : rest) = toEnum (hex a * 16 + hex b) : unescape rest
    unescape (c : rest)           = c : unescape rest
    unescape []                   = []
    hex c | isDigit c = fromEnum c - fromEnum '0'
          | otherwise = 10 + fromEnum (lower c) - fromEnum 'a'
    lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

withDocument :: IORef ServerState -> Value -> Bool -> IO ()
withDocument state request refreshSchemas =
  case (documentUri request, documentText request) of
    (Just uri, Just text) -> do
      modifyIORef' state (\s -> s { openDocuments = Map.insert uri text (openDocuments s) })
      when' refreshSchemas (fetchSchemasFor state uri text)
      publishDiagnostics state uri text
    _ -> return ()
  where
    when' condition action = if condition then action else return ()

-- | Fetch tool schemas for the servers a document declares. Done on open and
-- on save only: a network round trip per keystroke would hammer them.
fetchSchemasFor :: IORef ServerState -> String -> String -> IO ()
fetchSchemasFor state uri text = case runParser workflowParser (pathOfUri uri) text of
  Left _ -> return ()
  Right workflow
    | null (wfServers workflow) -> return ()
    | otherwise -> do
        outcome <- try (connect (const (return ())) workflow)
        case outcome of
          Left (_ :: SomeException) -> return ()
          Right (Left _) -> return ()
          Right (Right (_, _, schemaMap)) ->
            modifyIORef' state
              (\s -> s { knownSchemas = Map.insert uri schemaMap (knownSchemas s) })

-- Diagnostics ----------------------------------------------------------------

publishDiagnostics :: IORef ServerState -> String -> String -> IO ()
publishDiagnostics state uri text = do
  current <- readIORef state
  let schemaMap = fromMaybe Map.empty (Map.lookup uri (knownSchemas current))
      found = case runParser workflowParser (pathOfUri uri) text of
        Left err -> [parseDiagnostic err]
        Right workflow -> case typeCheck schemaMap workflow of
          Right () -> []
          Left errors -> map (typeDiagnostic workflow text) errors
  notify "textDocument/publishDiagnostics"
    (object ["uri" .= uri, "diagnostics" .= found])
  where
    -- errorBundlePretty renders "file:line:col:", the offending line with a
    -- caret under it, then the explanation. Take the position from the
    -- first and the message from the last.
    parseDiagnostic err =
      let rendered = lines (errorBundlePretty err)
          located = case rendered of
            first : _ -> positionOf first
            []        -> (1, 1)
          explanation = [l | l <- rendered, not (null l), not (elem '|' l),
                             not (isLocation l)]
          (line, column) = located
      in diagnostic (line - 1) (column - 1) column
           (case explanation of
              []    -> "parse error"
              parts -> unwords parts)

    isLocation l = case reverse (splitOn ':' l) of
      "" : col : ln : _ -> all isDigit col && all isDigit ln && not (null col)
      _                 -> False

    positionOf l = case reverse (splitOn ':' l) of
      "" : col : ln : _ | all isDigit col, all isDigit ln, not (null col) ->
        (read ln, read col)
      _ -> (1, 1)

    splitOn c str = case break (== c) str of
      (before, _ : rest) -> before : splitOn c rest
      (before, [])       -> [before]

-- | Type errors carry the name of the step or output they came from, which
-- is enough to put them on the right line.
typeDiagnostic :: Workflow -> String -> TypeError -> Value
typeDiagnostic workflow text err =
  diagnostic (lineOf label - 1) 0 (lineWidth (lineOf label)) (describeError err)
  where
    label = subjectOf err

    lineOf name = fromMaybe 1 (lookup name (stepLines ++ outputLines))
    stepLines   = [(stepName s, stepLine s) | s <- allSteps (steps workflow)]
    outputLines = [("output " ++ outputName f, outputLine f) | f <- wfOutputs workflow]

    lineWidth n = case drop (n - 1) (lines text) of
      line : _ -> length line
      []       -> 80

    allSteps nodes = concatMap unpack nodes
    unpack (StepNode s)   = [s]
    unpack (BranchNode b) = allSteps (branchTrue b) ++ allSteps (branchFalse b)

subjectOf :: TypeError -> String
subjectOf err = case err of
  ServerNotFound s _   -> s
  ToolNotFound s _ _   -> s
  TooFewArgs s _ _ _   -> s
  TooManyArgs s _ _ _  -> s
  UnboundVar s _       -> s
  BadPath s _ _        -> s
  ApiNotFound s _      -> s
  UnknownMethod s _    -> s
  BodyNotAllowed s _   -> s

describeError :: TypeError -> String
describeError err = case err of
  ServerNotFound _ alias  -> "No server declared with the alias " ++ show alias
  ToolNotFound _ alias t  -> show t ++ " is not a tool on " ++ alias
  TooFewArgs _ t need got -> show t ++ " needs " ++ show need ++ " argument(s), given " ++ show got
  TooManyArgs _ t need got -> show t ++ " takes " ++ show need ++ " argument(s), given " ++ show got
  UnboundVar _ name       -> show name ++ " is not an input or an earlier step's binding"
  BadPath _ path reason   -> path ++ " " ++ reason
  ApiNotFound _ alias     -> "No API declared with the alias " ++ show alias
  UnknownMethod _ m       -> show m ++ " is not an HTTP method"
  BodyNotAllowed _ m      -> show m ++ " does not take a request body"

diagnostic :: Int -> Int -> Int -> String -> Value
diagnostic line startCol endCol text = object
  [ "range" .= object
      [ "start" .= object ["line" .= line, "character" .= startCol]
      , "end"   .= object ["line" .= line, "character" .= max endCol (startCol + 1)]
      ]
  , "severity" .= (1 :: Int)
  , "source"   .= ("agent-dsl" :: String)
  , "message"  .= text
  ]

-- Completion -----------------------------------------------------------------

completionsFor :: IORef ServerState -> Value -> IO [Value]
completionsFor state request = case (documentUri request, positionOf request) of
  (Just uri, Just (line, character)) -> do
    current <- readIORef state
    case Map.lookup uri (openDocuments current) of
      Nothing -> return []
      Just text -> do
        let prefix = textBefore text line character
            schemaMap = fromMaybe Map.empty (Map.lookup uri (knownSchemas current))
        recorded <- Shapes.loadShapes (pathOfUri uri)
        return (completeAt text prefix schemaMap recorded)
  _ -> return []
  where
    positionOf req = do
      params <- field "params" req
      position <- field "position" params
      (,) <$> intField "line" position <*> intField "character" position

textBefore :: String -> Int -> Int -> String
textBefore text line character = case drop line (lines text) of
  current : _ -> take character current
  []          -> ""

-- | What to offer, decided by what sits immediately before the cursor.
completeAt :: String -> String -> Map.Map String [Schema.ToolSchema]
           -> Map.Map String Value -> [Value]
completeAt document prefix schemaMap recorded
  | Just alias <- afterCallPrefix "tool:" prefix
  , not (null alias) = map (item 6) (toolNames alias)
  | endsWith "tool:" prefix = map (item 9) (declaredAliases "servers" document)
  | endsWith "api:"  prefix = map (item 9) (declaredAliases "apis" document)
  | Just alias <- afterCallPrefix "api:" prefix
  , not (null alias) =
      map (item 2) (["get", "post", "put", "patch", "delete", "head"] :: [String])
  | endsWith "." prefix = pathCompletions
  | otherwise =
      map (item 14) (["step", "if", "else", "output", "input", "with"] :: [String])
  where
    pathCompletions = case pathBeforeCursor prefix of
      Nothing -> []
      Just (base, selectors) -> case shapeOf base of
        Nothing    -> []
        Just shape -> map (item 5) (fieldsAt shape selectors)

    shapeOf base = case bindingSource document base of
      Just (alias, tool)
        | Just schemas <- Map.lookup alias schemaMap
        , Just schema <- lookupTool tool schemas -> Just (resultSchema schema)
      _ -> Map.lookup base recorded

    lookupTool tool = TypeCheck.findTool `flip` tool

    toolNames alias = case Map.lookup alias schemaMap of
      Just schemas -> sort (map Schema.toolName schemas)
      Nothing      -> []

    item :: Int -> String -> Value
    item kind name = object ["label" .= name, "kind" .= kind]

-- | Aliases declared under a @servers:@ or @apis:@ block, read from the
-- text so that an unfinished document still completes.
declaredAliases :: String -> String -> [String]
declaredAliases blockName document = dedupe (sort (collect False (lines document)))
  where
    dedupe (a : b : rest) | a == b = dedupe (b : rest)
    dedupe (a : rest)              = a : dedupe rest
    dedupe []                      = []

    collect _ [] = []
    collect inside (line : rest)
      | (blockName ++ ":") `isPrefixOf` dropWhile (== ' ') line = collect True rest
      | isBlockStart line = collect False rest
      | inside, Just alias <- aliasOf line = alias : collect True rest
      | otherwise = collect inside rest

    -- A block ends at the next one. "workflow W:" starts with the word,
    -- not with "workflow:", so compare the first word rather than a prefix.
    isBlockStart line = case words line of
      first : _ -> takeWhile (/= ':') first
                     `elem` ["servers", "apis", "workflow", "input", "output"]
      []        -> False

    aliasOf line = case break (== '=') (dropWhile (== ' ') line) of
      (name, '=' : _) ->
        let alias = takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') name
        in if null alias then Nothing else Just alias
      _ -> Nothing

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

-- | @tool:docs.@ or @api:gh.@ -- the alias, when the cursor sits after its dot.
afterCallPrefix :: String -> String -> Maybe String
afterCallPrefix marker prefix
  | not ("." `isSuffixOf` prefix) = Nothing
  | otherwise =
      let withoutDot = init prefix
          alias = reverse (takeWhile isAliasChar (reverse withoutDot))
          before = take (length withoutDot - length alias) withoutDot
      in if marker `isSuffixOf` before then Just alias else Nothing
  where
    isAliasChar c = isAlphaNum c || c == '_' || c == '-'

-- | The binding and selectors already typed, for a cursor sitting after a dot.
pathBeforeCursor :: String -> Maybe (String, [Selector])
pathBeforeCursor prefix = case reverse (takeWhile isPathChar (reverse (init prefix))) of
  ""   -> Nothing
  path -> case splitPath path of
    (base, selectors) | not (null base) -> Just (base, selectors)
    _ -> Nothing
  where
    isPathChar c = isAlphaNum c || c `elem` ("._-[]" :: String)

splitPath :: String -> (String, [Selector])
splitPath path = (base, parseSelectors rest)
  where
    (base, rest) = span (\c -> isAlphaNum c || c == '_' || c == '-') path

    parseSelectors ('.' : more) =
      let (name, remaining) = span (\c -> isAlphaNum c || c == '_' || c == '-') more
      in Key name : parseSelectors remaining
    parseSelectors ('[' : more) =
      let (digits, remaining) = span isDigit more
      in Index (read ('0' : digits)) : parseSelectors (drop 1 remaining)
    parseSelectors _ = []

-- | Which server and tool produced a binding, read straight from the text so
-- that an incomplete document still completes.
bindingSource :: String -> String -> Maybe (String, String)
bindingSource document base = case mapMaybe parseLine (lines document) of
  found : _ -> Just found
  []        -> Nothing
  where
    parseLine line = do
      let stripped = dropWhile (== ' ') line
      afterBind <- suffixAfter "->" stripped
      let bound = takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
                    (dropWhile (== ' ') afterBind)
      if bound /= base then Nothing else do
        afterTool <- suffixAfter "tool:" stripped
        let alias = takeWhile (/= '.') afterTool
            tool = takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
                     (drop 1 (dropWhile (/= '.') afterTool))
        if null alias || null tool then Nothing else Just (alias, tool)

    suffixAfter marker text = go text
      where
        go [] = Nothing
        go rest@(_ : more)
          | marker `isPrefixOf` rest = Just (drop (length marker) rest)
          | otherwise = go more

-- | The field names available at the end of a path through a JSON Schema.
fieldsAt :: Value -> [Selector] -> [String]
fieldsAt schema [] = case field "properties" schema of
  Just (Object properties) -> sort (map (Key.toString . fst) (KeyMap.toList properties))
  _                        -> []
fieldsAt schema (Key name : rest) = case field "properties" schema of
  Just (Object properties) -> case KeyMap.lookup (Key.fromString name) properties of
    Just inner -> fieldsAt inner rest
    Nothing    -> []
  _ -> []
fieldsAt schema (Index _ : rest) = case field "items" schema of
  Just items -> fieldsAt items rest
  Nothing    -> []
