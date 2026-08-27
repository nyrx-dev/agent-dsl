{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map

import Data.Aeson (Value (..), decode, object, (.=))

import AST
import Interpreter (buildArgs, resolveInput)
import Parser (workflowParser)
import Schema hiding (toolName)
import qualified Schema
import TypeCheck


-- A tiny runner, so the suite needs no dependency beyond what the
-- executable already builds against.

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)

  let it name ok
        | ok        = putStrLn ("  ok   " ++ name)
        | otherwise = putStrLn ("  FAIL " ++ name) >> modifyIORef' failures (+ 1)

  putStrLn "Parser"
  mapM_ (uncurry it) parserTests

  putStrLn "TypeCheck"
  mapM_ (uncurry it) typeCheckTests

  putStrLn "Schema"
  mapM_ (uncurry it) schemaTests

  putStrLn "Interpreter"
  mapM_ (uncurry it) interpreterTests

  count <- readIORef failures
  putStrLn (show count ++ " failing")
  unless (count == 0) exitFailure


-- Parser ---------------------------------------------------------------------

parseWorkflow :: String -> Either String Workflow
parseWorkflow source = case runParser workflowParser "test" source of
  Left err -> Left (errorBundlePretty err)
  Right wf -> Right wf

parses :: String -> Bool
parses = either (const False) (const True) . parseWorkflow

demo :: [String] -> String
demo = unlines

parserTests :: [(String, Bool)]
parserTests =
  [ ( "parses a workflow with servers and a step"
    , case parseWorkflow simpleSource of
        Right wf -> workflowName wf == "Demo"
                 && map serverAlias (wfServers wf) == ["docs"]
                 && length (steps wf) == 1
        Left _   -> False
    )

  , ( "accepts leading whitespace before the first token"
    , parses ("\n\n" ++ simpleSource)
    )

  , ( "rejects trailing text it cannot parse"
    , not (parses (simpleSource ++ " this is not a step\n"))
    )

  , ( "keeps a carriage return out of a server url"
    , case parseWorkflow (toCRLF simpleSource) of
        Right wf -> map serverUrl (wfServers wf) == ["https://example.test/mcp"]
        Left _   -> False
    )

  , ( "accepts hyphens in tool names"
    , case parseWorkflow simpleSource of
        Right wf -> [toolName s | StepNode s <- steps wf] == ["resolve-library-id"]
        Left _   -> False
    )

  , ( "unescapes \\\" and \\\\ inside a string literal"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " step a = tool:docs.t(\"say \\\"hi\\\" \\\\ ok\") -> r"
        ]) of
        Right wf -> [l | StepNode s <- steps wf, Lit l <- stepInputs s]
                      == ["say \"hi\" \\ ok"]
        Left _   -> False
    )

  , ( "rejects a string literal spanning two lines"
    , not (parses (demo
        [ "workflow Demo:"
        , " step a = tool:docs.t(\"unterminated"
        , " still going\") -> r"
        ]))
    )

  , ( "parses a call with no arguments"
    , case parseWorkflow (demo ["workflow Demo:", " step a = tool:docs.t() -> r"]) of
        Right wf -> [length (stepInputs s) | StepNode s <- steps wf] == [0]
        Left _   -> False
    )

  , ( "ends a branch at the first node back at the if's indentation"
    , case parseWorkflow (branchSource ++ " step d = tool:docs.search(\"q\") -> after\n") of
        Right wf -> case steps wf of
          [StepNode _, BranchNode b, StepNode d] ->
            bindsTo d == "after"
              && length (branchTrue b) == 1
              && length (branchFalse b) == 1
          _ -> False
        Left _ -> False
    )

  , ( "parses both arms of a branch"
    , case parseWorkflow branchSource of
        Right wf -> case steps wf of
          [StepNode _, BranchNode b] -> length (branchTrue b) == 1
                                     && length (branchFalse b) == 1
          _                          -> False
        Left _ -> False
    )
  ]
  where
    toCRLF = concatMap (\c -> if c == '\n' then "\r\n" else [c])

simpleSource :: String
simpleSource = unlines
  [ "servers:"
  , " docs = https://example.test/mcp"
  , ""
  , "workflow Demo:"
  , " step a = tool:docs.resolve-library-id(\"react\", \"react\") -> libId"
  ]

branchSource :: String
branchSource = unlines
  [ "workflow Demo:"
  , " step a = tool:docs.search(\"q\") -> hits"
  , " if hits:"
  , "  step b = tool:docs.search(\"q\") -> found"
  , " else:"
  , "  step c = tool:docs.search(\"q\") -> missing"
  ]


-- TypeCheck ------------------------------------------------------------------

testSchemas :: Map.Map String [ToolSchema]
testSchemas = Map.fromList
  [ ("docs", [ ToolSchema { Schema.toolName = "search", requiredArgs = ["query", "limit"] }
             , ToolSchema { Schema.toolName = "resolve-library-id", requiredArgs = ["query", "libraryName"] }
             ])
  ]

-- | Constructor names of the reported errors, in order.
errorsIn :: String -> [String]
errorsIn source = case parseWorkflow source of
  Left _   -> ["<parse error>"]
  Right wf -> case typeCheck testSchemas wf of
    Right () -> []
    Left errs -> map (takeWhile (/= ' ') . show) errs

typeCheckTests :: [(String, Bool)]
typeCheckTests =
  [ ( "accepts a well-formed step"
    , errorsIn (step "docs" "search" "\"q\", \"10\"") == []
    )

  , ( "reports an unknown server alias"
    , errorsIn (step "nope" "search" "\"q\", \"10\"") == ["ServerNotFound"]
    )

  , ( "reports an unknown tool"
    , errorsIn (step "docs" "missing" "\"q\"") == ["ToolNotFound"]
    )

  , ( "reports too few arguments"
    , errorsIn (step "docs" "search" "\"q\"") == ["TooFewArgs"]
    )

  , ( "reports arguments the schema cannot take"
    , errorsIn (step "docs" "search" "\"q\", \"10\", \"extra\"") == ["TooManyArgs"]
    )

  , ( "reports a reference to a binding that is never defined"
    , errorsIn (step "docs" "search" "typo, \"10\"") == ["UnboundVar"]
    )

  , ( "accepts a reference to an earlier step's binding"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , " step b = tool:docs.search(hits, \"10\") -> more"
        ]) == []
    )

  , ( "reports a forward reference to a later binding"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(later, \"10\") -> hits"
        , " step b = tool:docs.search(\"q\", \"10\") -> later"
        ]) == ["UnboundVar"]
    )

  , ( "reports an undefined branch condition"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " if missing:"
        , " else:"
        ]) == ["UnboundVar"]
    )

  , ( "carries bindings out of both branch arms"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , " if hits:"
        , "  step b = tool:docs.search(\"q\", \"10\") -> fromTrue"
        , " else:"
        , "  step c = tool:docs.search(\"q\", \"10\") -> fromFalse"
        , " step d = tool:docs.search(fromTrue, fromFalse) -> done"
        ]) == []
    )

  , ( "reports every error, not just the first"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:nope.search(\"q\", \"10\") -> one"
        , " step b = tool:docs.search(\"q\") -> two"
        ]) == ["ServerNotFound", "TooFewArgs"]
    )
  ]
  where
    step alias tool args = unlines
      [ "workflow Demo:"
      , " step a = tool:" ++ alias ++ "." ++ tool ++ "(" ++ args ++ ") -> r"
      ]


-- Schema ---------------------------------------------------------------------

schemaTests :: [(String, Bool)]
schemaTests =
  [ ( "leaves a plain JSON body alone"
    , sseEvents (BL.pack "{\"id\":1}") == [BL.pack "{\"id\":1}"]
    )

  , ( "splits an SSE body into one payload per event"
    , sseEvents (BL.pack sseStream) == map BL.pack ["{\"a\":1}", "{\"b\":2}"]
    )

  , ( "tolerates CRLF line endings in an SSE body"
    , sseEvents (BL.pack "event: message\r\ndata: {\"a\":1}\r\n\r\n")
        == [BL.pack "{\"a\":1}"]
    )

  , ( "skips keep-alives and notifications to find the matching response"
    , rpcResult 2 (BL.pack noisyStream)
        == Right (object ["id" .= (2 :: Int), "ok" .= True])
    )

  , ( "reports a JSON-RPC error rather than returning it as a result"
    , rpcResult 1 (BL.pack "data: {\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Tool nope not found\"}}")
        == Left "Tool nope not found (code -32602)"
    )

  , ( "reports a tool-level isError result as a failure"
    , rpcResult 1 (BL.pack toolErrorStream)
        == Left "tool error: could not reach the index"
    )

  , ( "refuses a response answering a different request id"
    , isLeft (rpcResult 9 (BL.pack "data: {\"id\":1,\"result\":{}}"))
    )

  , ( "reads required arguments off a tool schema"
    , fmap (map requiredArgs . tools) (decode toolsListBody :: Maybe ToolsListResult)
        == Just [["query"], []]
    )
  ]
  where
    isLeft = either (const True) (const False)

sseStream :: String
sseStream = "event: message\ndata: {\"a\":1}\n\nevent: message\ndata: {\"b\":2}\n\n"

noisyStream :: String
noisyStream = concat
  [ ": ping\n\n"
  , "event: message\ndata: {\"method\":\"notifications/message\",\"params\":{}}\n\n"
  , "event: message\ndata: {\"id\":2,\"ok\":true}\n\n"
  ]

toolErrorStream :: String
toolErrorStream =
  "event: message\ndata: {\"id\":1,\"result\":{\"isError\":true,"
  ++ "\"content\":[{\"type\":\"text\",\"text\":\"could not reach the index\"}]}}\n\n"

toolsListBody :: BL.ByteString
toolsListBody = BL.pack
  "{\"id\":1,\"result\":{\"tools\":[{\"name\":\"search\",\"inputSchema\":{\"required\":[\"query\"]}},\
  \{\"name\":\"ping\",\"inputSchema\":{}}]}}"


-- Interpreter ----------------------------------------------------------------

interpreterTests :: [(String, Bool)]
interpreterTests =
  [ ( "resolves a literal to a JSON string"
    , resolveInput Map.empty (Lit "hi") == String "hi"
    )

  , ( "resolves a bound variable to the value it holds"
    , resolveInput (Map.fromList [("hits", Bool True)]) (Var "hits") == Bool True
    )

  , ( "names arguments after the schema's required parameters"
    , buildArgs ["query", "limit"] [String "react", String "10"]
        == object ["query" .= ("react" :: String), "limit" .= ("10" :: String)]
    )
  ]
