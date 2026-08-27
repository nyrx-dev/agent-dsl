module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import AST

type Parser = Parsec Void String

identifier :: Parser String
identifier = do
  first <- letterChar
  rest  <- many (alphaNumChar <|> char '_' <|> char '-')
  return (first : rest)

stringLiteral :: Parser String
stringLiteral = do
  _ <- char '"'
  content <- many (noneOf "\"")
  _ <- char '"'
  return content

spaceConsumer :: Parser ()
spaceConsumer = space

lexeme :: Parser a -> Parser a
lexeme p = do
  result <- p
  spaceConsumer
  return result

keyword :: String -> Parser ()
keyword kw = lexeme $ do
  _ <- string kw
  notFollowedBy alphaNumChar

symbol :: String -> Parser String
symbol s = lexeme (string s)

urlParser :: Parser String
urlParser = lexeme (some (noneOf " \t\r\n"))

serverDeclParser :: Parser ServerDecl
serverDeclParser = do
  aliasName <- lexeme identifier
  _ <- symbol "="
  url <- urlParser
  return ServerDecl { serverAlias = aliasName, serverUrl = url }

serversBlockParser :: Parser [ServerDecl]
serversBlockParser = do
  keyword "servers"
  _ <- symbol ":"
  many (try serverDeclParser)

argParser :: Parser Input
argParser = (Lit <$> lexeme stringLiteral) <|> (Var <$> lexeme identifier)

stepParser :: Parser Step
stepParser = do
  keyword "step"
  name <- lexeme identifier
  _ <- symbol "="
  keyword "tool"
  _ <- symbol ":"
  serverRef <- lexeme identifier
  _ <- symbol "."
  tool <- lexeme identifier
  _ <- symbol "("
  args <- sepBy argParser (symbol ",")
  _ <- symbol ")"
  _ <- symbol "->"
  bind <- lexeme identifier
  return Step
    { stepName = name
    , server = serverRef
    , toolName = tool
    , stepInputs = args
    , bindsTo = bind
    }

branchParser :: Parser Branch
branchParser = do
  keyword "if"
  cond <- lexeme identifier
  _ <- symbol ":"
  trueNodes <- many nodeParser
  keyword "else"
  _ <- symbol ":"
  falseNodes <- many nodeParser
  return Branch
    { branchCondition = Var cond
    , branchTrue = trueNodes
    , branchFalse = falseNodes
    }

nodeParser :: Parser Node
nodeParser = (StepNode <$> stepParser) <|> (BranchNode <$> branchParser)

workflowParser :: Parser Workflow
workflowParser = do
  spaceConsumer
  serverDecls <- option [] (try serversBlockParser)
  keyword "workflow"
  name <- lexeme identifier
  _ <- symbol ":"
  nodes <- many nodeParser
  eof
  return Workflow { workflowName = name, wfServers = serverDecls, steps = nodes }
