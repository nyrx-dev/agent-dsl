module AST where

-- | One step of a path into a bound value: a field of an object, or a
-- position in an array.
data Selector
  = Key String
  | Index Int
  deriving (Show, Eq)

data Input =
    Var String [Selector] |
    Lit String
    deriving (Show, Eq)

-- | Where a credential comes from. Only the environment, so that a .agent
-- file never carries a secret of its own.
newtype Secret = FromEnv String
    deriving (Show, Eq)

-- | How to present a credential to a server.
data Auth
  = NoAuth
  | BearerAuth Secret
  | HeaderAuth String Secret
  deriving (Show, Eq)

data ServerDecl = ServerDecl {
    serverAlias :: String,
    serverUrl :: String,
    serverAuth :: Auth
} deriving (Show)

data ApiDecl = ApiDecl {
    apiAlias :: String,
    apiBaseUrl :: String,
    apiAuth :: Auth
} deriving (Show)

-- | What a step calls: a tool on an MCP server, or a plain HTTP endpoint.
-- An API call builds its path by joining its arguments onto the base URL,
-- and may carry a request body.
data Call
  = ToolCall String String [Input]
  | ApiCall String String [Input] (Maybe Input)
  deriving (Show, Eq)

data Step = Step {
    stepName :: String,
    stepCall :: Call,
    bindsTo :: String
} deriving (Show)

-- | The inputs a call reads, wherever they appear in it.
callInputs :: Call -> [Input]
callInputs (ToolCall _ _ args)          = args
callInputs (ApiCall _ _ parts body)     = parts ++ maybe [] pure body

data Node  = StepNode Step | BranchNode Branch
    deriving (Show)

data CompareOp = Equals | NotEquals
    deriving (Show, Eq)

-- | What decides which arm of a branch runs. A bare input is a truthiness
-- test; the comparisons match JSON values, so a string literal only equals
-- a JSON string.
data Condition
  = Truthy Input
  | Compare CompareOp Input Input
  deriving (Show, Eq)

data Branch = Branch {
    branchCondition :: Condition,
    branchTrue :: [Node],
    branchFalse :: [Node]
} deriving (Show)

-- | One named value a workflow reports when it finishes.
data OutputField = OutputField {
    outputName :: String,
    outputValue :: Input
} deriving (Show)

data Workflow = Workflow {
    workflowName :: String,
    wfServers :: [ServerDecl],
    wfApis :: [ApiDecl],
    steps :: [Node],
    wfOutputs :: [OutputField]
} deriving (Show)

-- | How a reference is written in source, for error messages.
renderInput :: Input -> String
renderInput (Lit s)              = show s
renderInput (Var name selectors) = name ++ concatMap render selectors
  where
    render (Key key) = "." ++ key
    render (Index i) = "[" ++ show i ++ "]"
