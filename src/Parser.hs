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
  content <- many stringChar
  _ <- char '"'
  return content
  where
    stringChar = (char '\\' >> escapedChar) <|> noneOf "\"\\\n\r"
    escapedChar = choice
      [ '"'  <$ char '"'
      , '\\' <$ char '\\'
      , '\n' <$ char 'n'
      , '\t' <$ char 't'
      ]

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

selectorParser :: Parser Selector
selectorParser =
      (Key <$> (char '.' *> identifier))
  <|> (Index <$> (char '[' *> index <* char ']'))
  where
    index = read <$> some digitChar

-- | A reference to a binding, optionally reaching into it: @answer@,
-- @answer.content[0].text@. Selectors bind tightly, so no space is allowed
-- around them.
varParser :: Parser Input
varParser = lexeme (Var <$> identifier <*> many selectorParser)

argParser :: Parser Input
argParser = (Lit <$> lexeme stringLiteral) <|> varParser

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

currentColumn :: Parser Pos
currentColumn = sourceColumn <$> getSourcePos

-- | A node belonging to a branch arm, which must be indented past the column
-- the 'if' starts at. Without this the arms are unbounded and swallow every
-- node that follows the branch.
nodeIndentedBeyond :: Pos -> Parser Node
nodeIndentedBeyond column = do
  here <- currentColumn
  if here > column
    then nodeParser
    else fail "expected a step indented under the branch"

branchParser :: Parser Branch
branchParser = do
  column <- currentColumn
  keyword "if"
  cond <- varParser
  _ <- symbol ":"
  trueNodes <- many (nodeIndentedBeyond column)
  keyword "else"
  _ <- symbol ":"
  falseNodes <- many (nodeIndentedBeyond column)
  return Branch
    { branchCondition = cond
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
