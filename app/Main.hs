module Main where

    import Evaluator
    import GlobalState (emptyState, emptyStack)
    import Prettier 
    import Syntax

    -- evaluate commands
    eval :: Stm -> IO ()
    eval c = do 
        let (steps, err) = cEvalBig c emptyState emptyStack 
        printPretty steps
        putStrLn err
        putStr "\n"

    -- run test cases
    main :: IO ()
    main = do 
        -- skip
        eval Skip

        -- assignment
        eval $ "x" `Assign` Num 10;

        -- sequences
        let c1 = "x" `Assign` Num 3
        let c2 = "x" `Assign` Num 7
        eval $ Seq c1 c2;

        -- if 
        let c1 = "x" `Assign` Num 3
        let b  = Var "x" `Less` Num 5
        let t  = "x" `Assign` (Var "x" `Add` Num 1)
        let f  = "x" `Assign` (Var "x" `Sub` Num 1) 
        eval (Seq c1 (If b t f));

        -- while
        let c1 = "x" `Assign` Num 1
        let b  = Var "x" `Less` Num 5
        let c2 = "x" `Assign` (Var "x" `Add` Num 1)
        eval (Seq c1 (While b c2))

        -- error: variable not in scope
        let c1 = "x" `Assign` Num 3
        let b  = Var "x" `Equal` Num 5
        let t  = "x" `Assign` (Var "x" `Add` Num 1)
        let f  = "x" `Assign` (Var "y" `Sub` Num 1) 
        eval (Seq c1 (If b t f));

        -- error: division by 0
        let c1 = "x" `Assign` Num 4
        let c2 = "y" `Assign` Num 2
        let c = Seq c1 c2
        let b  = Var "x" `Greater` Num 0
        let c3 = "x" `Assign` (Var "x" `Div` Var "y")
        let c4 = "y" `Assign` (Var "y" `Sub` Num 1)
        let c' = Seq c3 c4
        eval (Seq c (While b c'))