module Evaluator where 
  
  import Syntax
  import GlobalState

  import Data.Either

  aEval :: AExp -> State -> Either String AExp
  aEval e s = case e of 
    Num n              -> Right e  
    Var x              -> case lookUp s x of 
                            Just n  -> Right $ Num n
                            Nothing -> Left "Free variable"
    Add e1 e2          -> do 
                            Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ Num $ n1 + n2
    Sub e1 e2          -> do 
                            Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ Num $ n1 - n2
    Mult e1 e2         -> do 
                            Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ Num $ n1 * n2

  bEval :: BExp -> State -> Either String BExp
  bEval e s = case e of 
    Tru                -> Right Tru
    Fls                -> Right Fls
    Equal e1 e2        -> do
                            Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ if n1 == n2 then Tru else Fls
    Less e1 e2         -> do 
                            Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ if n1 < n2 then Tru else Fls
    And e1 e2          -> do 
                            b1 <- bEval e1 s 
                            case b1 of 
                              Fls -> Right Fls
                              Tru -> bEval e2 s
    Or e1 e2           -> do 
                            b1 <- bEval e1 s 
                            case b1 of 
                              Tru -> Right Tru
                              Fls -> bEval e2 s 

  cEval :: Stm -> State -> Either String State
  cEval c s = case c of 
    Skip               -> Right s
    Seq c1 c2          -> do 
                            s' <- cEval c1 s 
                            cEval c2 s'
    If b c1 c2         -> do 
                            b' <- bEval b s
                            case b' of 
                              Tru -> cEval c1 s
                              Fls -> cEval c2 s
    Assign x e         -> do 
                            Num n <- aEval e s 
                            return $ update s x n
    While b c          -> do 
                            b' <- bEval b s 
                            case b' of 
                              Tru -> cEval (Seq c $ While b c) s 
                              Fls -> Right s
                               