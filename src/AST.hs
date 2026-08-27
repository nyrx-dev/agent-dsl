module AST where

data Input =
    Var String |
    Lit String
    deriving (Show)

data Step = Step {
    stepName :: String,
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
    steps :: [Node]
} deriving (Show)


exampleWorkflow :: Workflow
exampleWorkflow = Workflow
  { workflowName = "ResearchBot"
  , steps =
      [ StepNode $ Step { stepName = "search", toolName = "web_search", stepInputs = [Var "query"], bindsTo = "results" }
      , StepNode $ Step { stepName = "summarize", toolName = "llm", stepInputs = [Var "results"], bindsTo = "summary" }
      ]
  }
