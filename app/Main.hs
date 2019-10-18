module Main where
import Parser
import System.Environment

main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
