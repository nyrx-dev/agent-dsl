module Main where

import AST
import Parser
import qualified Schema
import TypeCheck
import Interpreter
import Data.Aeson (decode, Value)
import qualified Data.Map as Map

main :: IO ()
main = print exampleWorkflow
