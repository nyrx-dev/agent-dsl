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

-- | @env("NAME")@. A credential is only ever named, never written out, so
-- that a workflow file can be committed as it stands.
secretParser :: Parser Secret
secretParser = do
  keyword "env"
  _ <- symbol "("
  name <- lexeme stringLiteral
  _ <- symbol ")"
  return (FromEnv name)

-- | An optional credential clause after a declaration's URL:
-- @bearer env("TOKEN")@ or @header "X-API-Key" env("KEY")@.
authParser :: Parser Auth
authParser = option NoAuth (try bearer <|> try custom)
  where
    bearer = do
      keyword "bearer"
      BearerAuth <$> secretParser
    custom = do
      keyword "header"
      name <- lexeme stringLiteral
      HeaderAuth name <$> secretParser

serverDeclParser :: Parser ServerDecl
serverDeclParser = do
  aliasName <- lexeme identifier
  _ <- symbol "="
  url <- urlParser
  auth <- authParser
  return ServerDecl
    { serverAlias = aliasName, serverUrl = url, serverAuth = auth }

serversBlockParser :: Parser [ServerDecl]
serversBlockParser = do
  keyword "servers"
  _ <- symbol ":"
  many (try serverDeclParser)

apiDeclParser :: Parser ApiDecl
apiDeclParser = do
  aliasName <- lexeme identifier
  _ <- symbol "="
  url <- urlParser
  auth <- authParser
  return ApiDecl
    { apiAlias = aliasName, apiBaseUrl = url, apiAuth = auth }

apisBlockParser :: Parser [ApiDecl]
apisBlockParser = do
  keyword "apis"
  _ <- symbol ":"
  many (try apiDeclParser)

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

toolCallParser :: Parser Call
toolCallParser = do
  keyword "tool"
  _ <- symbol ":"
  serverRef <- lexeme identifier
  _ <- symbol "."
  tool <- lexeme identifier
  args <- parenthesised
  return (ToolCall serverRef tool args)

-- | @api:github.get("/repos/", owner, "/react")@, whose arguments join onto
-- the base URL to make the path, optionally followed by @with <body>@.
apiCallParser :: Parser Call
apiCallParser = do
  keyword "api"
  _ <- symbol ":"
  apiRef <- lexeme identifier
  _ <- symbol "."
  method <- lexeme identifier
  parts <- parenthesised
  body <- option Nothing (Just <$> (keyword "with" *> argParser))
  return (ApiCall apiRef method parts body)

parenthesised :: Parser [Input]
parenthesised = do
  _ <- symbol "("
  args <- sepBy argParser (symbol ",")
  _ <- symbol ")"
  return args

stepParser :: Parser Step
stepParser = do
  line <- currentLine
  keyword "step"
  name <- lexeme identifier
  _ <- symbol "="
  call <- toolCallParser <|> apiCallParser
  _ <- symbol "->"
  bind <- lexeme identifier
  return Step { stepName = name, stepCall = call, bindsTo = bind, stepLine = line }

currentColumn :: Parser Pos
currentColumn = sourceColumn <$> getSourcePos

currentLine :: Parser Int
currentLine = (unPos . sourceLine) <$> getSourcePos

-- | A node belonging to a branch arm, which must be indented past the column
-- the 'if' starts at. Without this the arms are unbounded and swallow every
-- node that follows the branch.
nodeIndentedBeyond :: Pos -> Parser Node
nodeIndentedBeyond column = do
  here <- currentColumn
  if here > column
    then nodeParser
    else fail "expected a step indented under the branch"

compareOpParser :: Parser CompareOp
compareOpParser = (Equals <$ symbol "==") <|> (NotEquals <$ symbol "!=")

conditionParser :: Parser Condition
conditionParser = do
  left <- argParser
  option (Truthy left) $ do
    op    <- compareOpParser
    right <- argParser
    return (Compare op left right)

branchParser :: Parser Branch
branchParser = do
  column <- currentColumn
  keyword "if"
  cond <- conditionParser
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

-- | The declaration blocks a workflow can open with, in either order.
data Declarations = DeclaredServers [ServerDecl] | DeclaredApis [ApiDecl]

declarationsParser :: Parser Declarations
declarationsParser =
      (DeclaredServers <$> try serversBlockParser)
  <|> (DeclaredApis    <$> try apisBlockParser)

-- | Words that begin a construct. An input or output name may not be one of
-- these, or a block would swallow the line that ends it.
reservedWords :: [String]
reservedWords =
  ["servers", "apis", "workflow", "input", "output", "step", "if", "else", "with"]

-- | An identifier that is not the start of some other construct.
plainIdentifier :: Parser String
plainIdentifier = try $ do
  name <- lexeme identifier
  if name `elem` reservedWords
    then fail (show name ++ " is a reserved word")
    else return name

-- | @city "which city to look up"@ -- a name the caller must supply, with
-- an optional description for whoever, or whatever, is calling.
inputFieldParser :: Parser InputField
inputFieldParser = do
  name <- plainIdentifier
  description <- option Nothing (Just <$> lexeme stringLiteral)
  return InputField { inputFieldName = name, inputFieldDescription = description }

inputsBlockParser :: Parser [InputField]
inputsBlockParser = do
  keyword "input"
  _ <- symbol ":"
  many (try inputFieldParser)

outputFieldParser :: Parser OutputField
outputFieldParser = do
  line <- currentLine
  name <- plainIdentifier
  _ <- symbol "="
  value <- argParser
  return OutputField { outputName = name, outputValue = value, outputLine = line }

-- | What the workflow reports when it finishes. Without this block every
-- binding is printed, which is rarely what a caller wants.
outputsBlockParser :: Parser [OutputField]
outputsBlockParser = do
  keyword "output"
  _ <- symbol ":"
  many (try outputFieldParser)

workflowParser :: Parser Workflow
workflowParser = do
  spaceConsumer
  blocks <- many declarationsParser
  keyword "workflow"
  name <- lexeme identifier
  _ <- symbol ":"
  inputs <- option [] (try inputsBlockParser)
  nodes <- many nodeParser
  outputs <- option [] (try outputsBlockParser)
  eof
  return Workflow
    { workflowName = name
    , wfServers    = concat [decls | DeclaredServers decls <- blocks]
    , wfApis       = concat [decls | DeclaredApis decls <- blocks]
    , wfInputs     = inputs
    , steps        = nodes
    , wfOutputs    = outputs
    }
