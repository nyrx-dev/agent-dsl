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

data Workflow = Workflow {
    workflowName :: String,
    wfServers :: [ServerDecl],
    steps :: [Node]
} deriving (Show)

-- | How a reference is written in source, for error messages.
renderInput :: Input -> String
renderInput (Lit s)              = show s
renderInput (Var name selectors) = name ++ concatMap render selectors
  where
    render (Key key) = "." ++ key
    render (Index i) = "[" ++ show i ++ "]"
