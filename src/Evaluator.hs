module Evaluator ( 
  eval
  ) where

  import Data.Either 

  import Syntax

  -- evaluate a term
  eval :: Term -> Either String Term
  eval (Num n)              = Right $ Num n            -- [E-INT]
  eval (Add t1 t2)          = do                       -- [E-ADD]
                                Num n1 <- eval t1
                                Num n2 <- eval t2
                                return $ Num $ n1 + n2
  eval (Mult t1 t2)         = do                        -- [E-MULT]
                                Num n1 <- eval t1
                                Num n2 <- eval t2
                                return $ Num $ n1 * n2 
  eval (Sub t1 t2)          = do                        -- [E-SUB]
                                Num n1 <- eval t1
                                Num n2 <- eval t2 
                                return $ Num $ n1 - n2  
  eval (Div _ (Num 0))      = Left "Divide by 0"        -- [E-DIV0]
  eval (Div t1 t2)          = do                        -- [E-DIV]
                                Num n1 <- eval t1
                                Num n2 <- eval t2 
                                return $ Num $ n1 `div` n2