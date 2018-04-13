module Evaluator ( 
  eval
  ) where

  import Data.Either 

  import Syntax

  eval :: Term -> Either String Term
  eval (Num n)          = Right $ Num n
  eval (Add t1 t2)      = do 
    Num n1 <- eval t1
    Num n2 <- eval t2
    return $ Num $ n1 + n2
  eval (Mult t1 t2)     = do 
    Num n1 <- eval t1
    Num n2 <- eval t2
    return $ Num $ n1 * n2 
  eval (Sub t1 t2)      = do 
    Num n1 <- eval t1
    Num n2 <- eval t2 
    return $ Num $ n1 - n2  
  eval (Div _ (Num 0)) = Left "Divide by 0"
  eval (Div t1 t2)     = do 
    Num n1 <- eval t1
    Num n2 <- eval t2 
    return $ Num $ n1 `div` n2