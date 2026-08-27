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

data ServerDecl = ServerDecl {
    serverAlias :: String,
    serverUrl :: String
} deriving (Show)

data Step = Step {
    stepName :: String,
    server :: String,
    toolName :: String,
    stepInputs :: [Input],
    bindsTo :: String
} deriving (Show)

data Node  = StepNode Step | BranchNode Branch
    deriving (Show)

data Branch = Branch {
    branchCondition :: Input,
    branchTrue :: [Node],
    branchFalse :: [Node]
} deriving (Show)

data Workflow = Workflow {
    workflowName :: String,
    wfServers :: [ServerDecl],
    steps :: [Node]
} deriving (Show)
