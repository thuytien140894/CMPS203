module Main where

  import Evaluator
  import Syntax

  interpret :: Term -> String 
  interpret t = show t ++ " => " ++ show (eval t) ++ "\n"

  main :: IO ()
  main = do 
    putStrLn $ interpret $ Num 5
    putStrLn $ interpret (Num 5 `Add` Num 3)
    putStrLn $ interpret (Num 5 `Mult` Num 10 `Add` Num 3)
    putStrLn $ interpret ((Num 5 `Mult` Num 10 `Div` Num 2) `Add` Num 3)
    putStrLn $ interpret (Num 5 `Mult` Num 3)
    putStrLn $ interpret (Num 3 `Sub` Num 10)
    putStrLn $ interpret (Num 9 `Div` Num 3)
    putStrLn $ interpret (Num (-11) `Div` Num 2)
    putStrLn $ interpret (Num 11 `Div` Num 2)
    putStrLn $ interpret ((Num (-9) `Div` Num (-2)) `Mult` (Num 8 `Add` Num 7) `Sub` Num 4)
    putStrLn $ interpret ((Num 9 `Mult` Num 3) `Add` (Num 7 `Sub` Num 9))
    putStrLn $ interpret ((Num 10 `Mult` Num 3) `Add` Num 2 `Add` Num 9 `Div` Num 0)
    putStrLn $ interpret (Num 9 `Div` Num 0)