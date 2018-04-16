module Evaluator where 
  
  import Syntax
  import GlobalState

  import Data.Maybe
  import Data.Functor
  import Control.Applicative

  import qualified Data.Map as Map (lookup)

  aEval :: AExp -> State -> Maybe AExp
  aEval e s = case e of 
    Num n              -> Just e  
    Var x              -> Num <$> lookUp s x 
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

  bEval :: BExp -> State -> Maybe BExp
  bEval e s = case e of 
    Tru                -> Just Tru
    Fls                -> Just Fls
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
                              Fls -> Just Fls
                              Tru -> bEval e2 s
    Or e1 e2           -> do 
                            b1 <- bEval e1 s 
                            case b1 of 
                              Tru -> Just Tru
                              Fls -> bEval e2 s 

  cEval :: Stm -> State -> Maybe State
  cEval c s = case c of 
    Skip               -> Just s
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
                              Fls -> Just s
                               