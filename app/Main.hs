module Main where

  import Evaluator
  import Syntax

  main :: IO ()
  main = do 
    print $ eval (Num 5)
    print $ eval (Num 10 `Add` Num 20)