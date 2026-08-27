module AST where

data Input =
    Var String |
    Lit String
    deriving (Show)

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
