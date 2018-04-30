module Evaluator where 
  
    import Syntax
    import GlobalState

    import Data.Either

    -- evaluate arithmetic expressions
    aEval :: AExp -> State -> Either String AExp
    aEval e s = case e of 
        Num n      -> Right e  
        Var x      -> case lookUp s x of 
                          Just n  -> Right $ Num n
                          Nothing -> Left $ "Variable not in scope: " ++ x
        Add e1 e2  -> do Num n1 <- aEval e1 s
                         Num n2 <- aEval e2 s
                         return $ Num $ n1 + n2
        Sub e1 e2  -> do Num n1 <- aEval e1 s
                         Num n2 <- aEval e2 s
                         return $ Num $ n1 - n2
        Mult e1 e2 -> do Num n1 <- aEval e1 s
                         Num n2 <- aEval e2 s
                         return $ Num $ n1 * n2
        Div e1 e2  -> do Num n1 <- aEval e1 s
                         Num n2 <- aEval e2 s 
                         if n2 == 0 
                            then Left "Division by zero"
                            else Right $ Num $ n1 `div` n2

    -- evaluate boolean expressions
    bEval :: BExp -> State -> Either String BExp
    bEval e s = case e of 
        Tru           -> Right Tru
        Fls           -> Right Fls
        Equal e1 e2   -> do Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ if n1 == n2 then Tru else Fls
        Less e1 e2    -> do Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ if n1 < n2 then Tru else Fls
        Greater e1 e2 -> do Num n1 <- aEval e1 s
                            Num n2 <- aEval e2 s
                            return $ if n1 > n2 then Tru else Fls
        Not e'       -> do b <- bEval e' s
                           return $ if b == Tru then Fls else Tru
        And e1 e2    -> do b1 <- bEval e1 s 
                           case b1 of 
                               Fls -> Right Fls
                               Tru -> bEval e2 s
        Or e1 e2     -> do b1 <- bEval e1 s 
                           case b1 of 
                               Tru -> Right Tru
                               Fls -> bEval e2 s 

    -- evaluate commands, small step                           
    cEvalSmall :: Stm -> State -> Either String (Stm, State)
    cEvalSmall c s = case c of 
        Skip        -> Right (Skip, s)
        Seq Skip c' -> return (c', s)
        Seq c1 c2   -> do (c1', s') <- cEvalSmall c1 s 
                          return (Seq c1' c2, s')
        If b c1 c2  -> do b' <- bEval b s
                          case b' of 
                              Tru -> return (c1, s)
                              Fls -> return (c2, s)
        Assign x e  -> do Num n <- aEval e s 
                          return (Skip, update s x n)
        While b c   -> do b' <- bEval b s 
                          case b' of 
                              Tru -> return (Seq c $ While b c, s) 
                              Fls -> return (Skip, s)

    -- evaluate commands, big step
    cEvalBig :: Stm -> State -> Stack -> (Stack, String)
    cEvalBig c s stack = do 
        let curStack = stack `push` (c, s)
        let res = cEvalSmall c s 
        case res of 
            Right (Skip, s') -> (curStack `push` (Skip, s'), "Done")
            Right (c', s')   -> cEvalBig c' s' curStack
            Left err         -> (curStack, err)