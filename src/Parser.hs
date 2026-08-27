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

stepParser :: Parser Step
stepParser = do
  keyword "step"
  name <- lexeme identifier
  _ <- symbol "="
  keyword "tool"
  _ <- symbol ":"
  tool <- lexeme identifier
  _ <- symbol "("
  args <- sepBy (lexeme identifier) (symbol ",")
  _ <- symbol ")"
  _ <- symbol "->"
  bind <- lexeme identifier
  return Step
    { stepName = name
    , toolName = tool
    , stepInputs = map Var args
    , bindsTo = bind
    }

workflowParser :: Parser Workflow
workflowParser = do
  keyword "workflow"
  name <- lexeme identifier
  _ <- symbol ":"
  nodes <- many nodeParser
  return Workflow { workflowName = name, steps = nodes }

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
