module Main where
import Evaluator
import Parser
import System.Environment

main = do
  getArgs >>= print . eval . readExpr . head
