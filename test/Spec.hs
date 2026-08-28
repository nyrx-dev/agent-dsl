{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map

import Data.Aeson (Value (..), decode, object, toJSON, (.=))

import AST
import Auth (resolveAuth)
import Data.List (isInfixOf)
import Interpreter (buildArgs, collectOutputs, evalCondition, isTruthy, resolveInput)
import System.Environment (setEnv, unsetEnv)
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

  putStrLn "Auth"
  authResults <- authTests
  mapM_ (uncurry it) authResults

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
        Right wf -> [tool | StepNode s <- steps wf, ToolCall _ tool _ <- [stepCall s]]
                      == ["resolve-library-id"]
        Left _   -> False
    )

  , ( "unescapes \\\" and \\\\ inside a string literal"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " step a = tool:docs.t(\"say \\\"hi\\\" \\\\ ok\") -> r"
        ]) of
        Right wf -> [l | StepNode s <- steps wf, Lit l <- callInputs (stepCall s)]
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

  , ( "parses a selector path on a binding reference"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " step a = tool:docs.t(answer.content[0].text) -> r"
        ]) of
        Right wf -> [callInputs (stepCall s) | StepNode s <- steps wf]
                      == [[Var "answer" [Key "content", Index 0, Key "text"]]]
        Left _   -> False
    )

  , ( "parses a call with no arguments"
    , case parseWorkflow (demo ["workflow Demo:", " step a = tool:docs.t() -> r"]) of
        Right wf -> [length (callInputs (stepCall s)) | StepNode s <- steps wf] == [0]
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

  , ( "parses an input block with descriptions"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " input:"
        , "  owner \"who owns the repo\""
        , "  repo"
        , " step a = tool:docs.search(owner, repo) -> hits"
        ]) of
        Right wf ->
          [ (inputFieldName f, inputFieldDescription f) | f <- wfInputs wf ]
            == [("owner", Just "who owns the repo"), ("repo", Nothing)]
        Left _ -> False
    )

  , ( "an input block stops at the first step rather than eating it"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " input:"
        , "  owner"
        , " step a = tool:docs.search(owner, \"x\") -> hits"
        ]) of
        Right wf -> length (wfInputs wf) == 1 && length (steps wf) == 1
        Left _   -> False
    )

  , ( "parses an output block naming what to report"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\") -> hits"
        , ""
        , "output:"
        , " first = hits.content[0].text"
        , " failed = hits.isError"
        ]) of
        Right wf ->
          [ (outputName f, outputValue f) | f <- wfOutputs wf ]
            == [ ("first", Var "hits" [Key "content", Index 0, Key "text"])
               , ("failed", Var "hits" [Key "isError"])
               ]
        Left _ -> False
    )

  , ( "a workflow with no output block names nothing"
    , case parseWorkflow simpleSource of
        Right wf -> null (wfOutputs wf)
        Left _   -> False
    )

  , ( "parses a bearer credential on a server declaration"
    , case parseWorkflow (demo
        [ "servers:"
        , " s = https://example.test/mcp bearer env(\"TOK\")"
        , ""
        , "workflow Demo:"
        ]) of
        Right wf -> map serverAuth (wfServers wf) == [BearerAuth (FromEnv "TOK")]
        Left _   -> False
    )

  , ( "parses a named header credential on an api declaration"
    , case parseWorkflow (demo
        [ "apis:"
        , " a = https://example.test header \"X-API-Key\" env(\"KEY\")"
        , ""
        , "workflow Demo:"
        ]) of
        Right wf -> map apiAuth (wfApis wf) == [HeaderAuth "X-API-Key" (FromEnv "KEY")]
        Left _   -> False
    )

  , ( "a declaration with no credential clause carries none"
    , case parseWorkflow simpleSource of
        Right wf -> map serverAuth (wfServers wf) == [NoAuth]
        Left _   -> False
    )

  , ( "parses an api step, joining its arguments into the path"
    , case parseWorkflow (demo
        [ "apis:"
        , " gh = https://api.github.com"
        , ""
        , "workflow Demo:"
        , " step a = api:gh.get(\"/repos/\", owner, \"/react\") -> r"
        ]) of
        Right wf -> [stepCall s | StepNode s <- steps wf]
          == [ApiCall "gh" "get"
                [Lit "/repos/", Var "owner" [], Lit "/react"] Nothing]
        Left _   -> False
    )

  , ( "parses a request body introduced by with"
    , case parseWorkflow (demo
        [ "workflow Demo:"
        , " step a = api:gh.post(\"/x\") with \"{}\" -> r"
        ]) of
        Right wf -> [stepCall s | StepNode s <- steps wf]
          == [ApiCall "gh" "post" [Lit "/x"] (Just (Lit "{}"))]
        Left _   -> False
    )

  , ( "accepts servers and apis blocks in either order"
    , case parseWorkflow (demo
        [ "apis:"
        , " gh = https://api.github.com"
        , "servers:"
        , " docs = https://example.test/mcp"
        , ""
        , "workflow Demo:"
        , " step a = api:gh.get(\"/x\") -> r"
        ]) of
        Right wf -> map apiAlias (wfApis wf) == ["gh"]
                 && map serverAlias (wfServers wf) == ["docs"]
        Left _   -> False
    )

  , ( "parses a bare condition as a truthiness test"
    , conditionOf " if flag:\n else:\n" == Just (Truthy (Var "flag" []))
    )

  , ( "parses an equality condition"
    , conditionOf " if answer.isError == \"no\":\n else:\n"
        == Just (Compare Equals (Var "answer" [Key "isError"]) (Lit "no"))
    )

  , ( "parses an inequality condition"
    , conditionOf " if a != b:\n else:\n"
        == Just (Compare NotEquals (Var "a" []) (Var "b" []))
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

conditionOf :: String -> Maybe Condition
conditionOf body = case parseWorkflow ("workflow Demo:\n" ++ body) of
  Right wf -> case steps wf of
    [BranchNode b] -> Just (branchCondition b)
    _              -> Nothing
  Left _ -> Nothing

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
  [ ("docs", [ ToolSchema
                 { Schema.toolName = "search"
                 , requiredArgs    = ["query", "limit"]
                 , outputSchema    = Nothing
                 }
             , ToolSchema
                 { Schema.toolName = "resolve-library-id"
                 , requiredArgs    = ["query", "libraryName"]
                 , outputSchema    = Nothing
                 }
             , ToolSchema
                 { Schema.toolName = "structured"
                 , requiredArgs    = ["query"]
                 , outputSchema    = Just (object
                     [ "type"       .= ("object" :: String)
                     , "properties" .= object
                         ["result" .= object ["type" .= ("string" :: String)]]
                     ])
                 }
             ])
  ]

-- | A workflow with one API declared and a single step making @call@.
apiWorkflow :: String -> String
apiWorkflow call = unlines
  [ "apis:"
  , " gh = https://api.github.com"
  , ""
  , "workflow Demo:"
  , " step a = " ++ call ++ " -> r"
  ]

-- | A workflow whose first step binds @source@, then feeds @path@ to a
-- second step.
throughPath :: String -> String -> String
throughPath source path = unlines
  [ "workflow Demo:"
  , " step a = tool:docs." ++ source ++ " -> out"
  , " step b = tool:docs.search(" ++ path ++ ", \"10\") -> more"
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

  , ( "accepts a path through the result envelope"
    , errorsIn (throughPath "search(\"q\", \"10\")" "out.content[0].text") == []
    )

  , ( "reports a misspelled field of the result envelope"
    , errorsIn (throughPath "search(\"q\", \"10\")" "out.contnt[0].text") == ["BadPath"]
    )

  , ( "reports reaching through a scalar"
    , errorsIn (throughPath "search(\"q\", \"10\")" "out.isError.text") == ["BadPath"]
    )

  , ( "reports indexing something that is not an array"
    , errorsIn (throughPath "structured(\"q\")" "out.structuredContent[0]") == ["BadPath"]
    )

  , ( "accepts a path the declared output schema does have"
    , errorsIn (throughPath "structured(\"q\")" "out.structuredContent.result") == []
    )

  , ( "reports a path the declared output schema does not have"
    , errorsIn (throughPath "structured(\"q\")" "out.structuredContent.nope") == ["BadPath"]
    )

  , ( "leaves structuredContent alone when no output schema is declared"
    , errorsIn (throughPath "search(\"q\", \"10\")" "out.structuredContent.anything") == []
    )

  , ( "does not check paths in a condition, which reads an absent one as null"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , " if hits.contnt:"
        , " else:"
        ]) == []
    )

  , ( "accepts a declared api called with a known method"
    , errorsIn (apiWorkflow "api:gh.get(\"/repos/facebook/react\")") == []
    )

  , ( "reports an api alias that is not declared"
    , errorsIn (apiWorkflow "api:nope.get(\"/x\")") == ["ApiNotFound"]
    )

  , ( "reports a method that is not an HTTP method"
    , errorsIn (apiWorkflow "api:gh.fetch(\"/x\")") == ["UnknownMethod"]
    )

  , ( "reports a body on a method that does not take one"
    , errorsIn (apiWorkflow "api:gh.get(\"/x\") with \"{}\"") == ["BodyNotAllowed"]
    )

  , ( "accepts a body on post"
    , errorsIn (apiWorkflow "api:gh.post(\"/x\") with \"{}\"") == []
    )

  , ( "checks binding references inside an api path"
    , errorsIn (apiWorkflow "api:gh.get(\"/x/\", nope)") == ["UnboundVar"]
    )

  , ( "leaves paths into an api result unchecked, having no schema for it"
    , errorsIn (unlines
        [ "apis:"
        , " gh = https://api.github.com"
        , ""
        , "workflow Demo:"
        , " step a = api:gh.get(\"/x\") -> out"
        , " step b = tool:docs.search(out.anything.at.all, \"10\") -> more"
        ]) == []
    )

  , ( "treats a declared input as bound from the first step"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " input:"
        , "  query"
        , " step a = tool:docs.search(query, \"10\") -> hits"
        ]) == []
    )

  , ( "still reports a name that is neither an input nor a binding"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " input:"
        , "  query"
        , " step a = tool:docs.search(query, other) -> hits"
        ]) == ["UnboundVar"]
    )

  , ( "reports an output naming a binding that does not exist"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , ""
        , "output:"
        , " x = nope.field"
        ]) == ["UnboundVar"]
    )

  , ( "reports an output reaching down a path the result cannot have"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , ""
        , "output:"
        , " x = hits.contnt"
        ]) == ["BadPath"]
    )

  , ( "accepts an output naming a binding from inside both branch arms"
    , errorsIn (unlines
        [ "workflow Demo:"
        , " step a = tool:docs.search(\"q\", \"10\") -> hits"
        , " if hits:"
        , "  step b = tool:docs.search(\"q\", \"10\") -> found"
        , " else:"
        , "  step c = tool:docs.search(\"q\", \"10\") -> found"
        , ""
        , "output:"
        , " x = found.isError"
        ]) == []
    )

  , ( "reports unbound names on both sides of a comparison"
    , errorsIn (unlines ["workflow Demo:", " if left == right:", " else:"])
        == ["UnboundVar", "UnboundVar"]
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
    , rpcResult 2 (BL.pack noisyStream) == Right (object ["ok" .= True])
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

  , ( "reports a response carrying neither a result nor an error"
    , isLeft (rpcResult 1 (BL.pack "data: {\"id\":1}"))
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
  , "event: message\ndata: {\"id\":2,\"result\":{\"ok\":true}}\n\n"
  ]

toolErrorStream :: String
toolErrorStream =
  "event: message\ndata: {\"id\":1,\"result\":{\"isError\":true,"
  ++ "\"content\":[{\"type\":\"text\",\"text\":\"could not reach the index\"}]}}\n\n"

toolsListBody :: BL.ByteString
toolsListBody = BL.pack
  "{\"tools\":[{\"name\":\"search\",\"inputSchema\":{\"required\":[\"query\"]}},\
  \{\"name\":\"ping\",\"inputSchema\":{}}]}"


-- Interpreter ----------------------------------------------------------------

interpreterTests :: [(String, Bool)]
interpreterTests =
  [ ( "resolves a literal to a JSON string"
    , resolveInput Map.empty (Lit "hi") == Right (String "hi")
    )

  , ( "resolves a bound variable to the value it holds"
    , resolveInput boundCtx (Var "hits" []) == Right (Bool True)
    )

  , ( "follows a selector path through objects and arrays"
    , resolveInput boundCtx (Var "answer" [Key "content", Index 0, Key "text"])
        == Right (String "hello")
    )

  , ( "fails on a selector the bound value does not have"
    , resolveInput boundCtx (Var "answer" [Key "missing"])
        == Left "answer.missing has no field \"missing\"; it has \"content\", \"isError\""
    )

  , ( "suggests the field that was probably meant"
    , resolveInput boundCtx (Var "answer" [Key "conten"])
        == Left "answer.conten has no field \"conten\"; did you mean \"content\"?"
    )

  , ( "lists what is there when nothing looks close"
    , resolveInput boundCtx (Var "answer" [Key "zzz"])
        == Left "answer.zzz has no field \"zzz\"; it has \"content\", \"isError\""
    )

  , ( "says what a value is when a path runs into a scalar"
    , resolveInput boundCtx (Var "hits" [Key "anything"])
        == Left "hits.anything is a boolean, not an object"
    )

  , ( "points at the selector that failed, not the whole path"
    , resolveInput boundCtx (Var "answer" [Key "content", Index 0, Key "txt"])
        == Left "answer.content[0].txt has no field \"txt\"; it has \"text\""
    )

  , ( "fails on an array index past the end"
    , isLeftE (resolveInput boundCtx (Var "answer" [Key "content", Index 4]))
    )

  , ( "names arguments after the schema's required parameters"
    , buildArgs ["query", "limit"] [String "react", String "10"]
        == object ["query" .= ("react" :: String), "limit" .= ("10" :: String)]
    )

  , ( "takes the else arm when a field is false"
    , not (evalCondition boundCtx (Truthy (Var "answer" [Key "isError"])))
    )

  , ( "reads a field that is not there as false rather than failing"
    , not (evalCondition boundCtx (Truthy (Var "answer" [Key "nope"])))
    )

  , ( "compares a selector against a string literal"
    , evalCondition boundCtx
        (Compare Equals (Var "answer" [Key "content", Index 0, Key "text"]) (Lit "hello"))
    )

  , ( "reads an inequality as the negation of the equality"
    , evalCondition boundCtx
        (Compare NotEquals (Var "answer" [Key "content", Index 0, Key "text"]) (Lit "other"))
    )

  , ( "reports only the values an output block names"
    , collectOutputs boundCtx
        [ OutputField "text" (Var "answer" [Key "content", Index 0, Key "text"]) ]
        == Right (object ["text" .= ("hello" :: String)])
    )

  , ( "reports every binding when no output is named"
    , keyed (collectOutputs boundCtx []) == Just ["answer", "hits"]
    )

  , ( "fails rather than reporting a value that is not there"
    , isLeftE (collectOutputs boundCtx [OutputField "x" (Var "answer" [Key "nope"])])
    )

  , ( "treats null, false, zero and empty containers as false"
    , not (any isTruthy [Null, Bool False, Number 0, String "", object [], toJSON ([] :: [Int])])
    )

  , ( "treats a non-empty value as true"
    , all isTruthy [Bool True, Number 1, String "x", object ["a" .= (1 :: Int)]]
    )
  ]
  where
    isLeftE = either (const True) (const False)
    keyed (Right (Object fields)) = Just (map (Key.toString . fst) (KeyMap.toAscList fields))
    keyed _                       = Nothing

boundCtx :: Map.Map String Value
boundCtx = Map.fromList
  [ ("hits", Bool True)
  , ("answer", object
      [ "content" .= [object ["text" .= ("hello" :: String)]]
      , "isError" .= False
      ])
  ]


-- Auth -----------------------------------------------------------------------

authTests :: IO [(String, Bool)]
authTests = do
  setEnv   "AGENT_DSL_TEST_TOKEN" "sekret"
  setEnv   "AGENT_DSL_TEST_EMPTY" ""
  unsetEnv "AGENT_DSL_TEST_ABSENT"

  bearer  <- resolveAuth "API x" (BearerAuth (FromEnv "AGENT_DSL_TEST_TOKEN"))
  named   <- resolveAuth "API x" (HeaderAuth "X-API-Key" (FromEnv "AGENT_DSL_TEST_TOKEN"))
  none    <- resolveAuth "API x" NoAuth
  absent  <- resolveAuth "API x" (BearerAuth (FromEnv "AGENT_DSL_TEST_ABSENT"))
  emptied <- resolveAuth "API x" (BearerAuth (FromEnv "AGENT_DSL_TEST_EMPTY"))

  return
    [ ( "a bearer secret becomes an Authorization header"
      , bearer == Right [("Authorization", "Bearer sekret")]
      )
    , ( "a named header carries the secret as its value"
      , named == Right [("X-API-Key", "sekret")]
      )
    , ( "a declaration without a credential contributes no headers"
      , none == Right []
      )
    , ( "an unset variable is reported by name"
      , absent == Left "API x: environment variable AGENT_DSL_TEST_ABSENT is not set"
      )
      -- setEnv removes a variable rather than setting it empty, so this
      -- only pins down that an empty one is refused and named. The wording
      -- of the empty case is covered by running against a real shell.
    , ( "a variable with no usable value is refused and named"
      , case emptied of
          Left message -> "AGENT_DSL_TEST_EMPTY" `isInfixOf` message
          Right _      -> False
      )
    , ( "a failure never quotes the secret itself"
      , not (any ("sekret" `isInfixOf`) [show absent, show emptied])
      )
    ]
