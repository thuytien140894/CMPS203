module Evaluator ( 
  eval
  ) where

  import Data.Maybe 

  import Syntax

  eval :: Term -> Maybe Term
  eval (Num n)      = Just $ Num n
  eval (Add t1 t2)  = do 
    Num n1 <- eval t1
    Num n2 <- eval t2
    return $ Num $ n1 + n2
  eval (Mult t1 t2) = do 
    Num n1 <- eval t1
    Num n2 <- eval t2
    return $ Num $ n1 * n2      