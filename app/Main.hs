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
        eval Skip

        eval $ "x" `Assign` Num 10;

        let c1 = "x" `Assign` Num 3
        let c2 = "x" `Assign` Num 7
        eval $ Seq c1 c2;

        let c1 = "x" `Assign` Num 3
        let b  = Var "x" `Less` Num 5
        let t  = "x" `Assign` (Var "x" `Add` Num 1)
        let f  = "x" `Assign` (Var "x" `Sub` Num 1) 
        eval (Seq c1 (If b t f));

        let c1 = "x" `Assign` Num 1
        let b  = Var "x" `Less` Num 5
        let c2 = "x" `Assign` (Var "x" `Add` Num 1)
        eval (Seq c1 (While b c2))

        let c1 = "x" `Assign` Num 3
        let b  = Var "x" `Equal` Num 5
        let t  = "x" `Assign` (Var "x" `Add` Num 1)
        let f  = "x" `Assign` (Var "y" `Sub` Num 1) 
        eval (Seq c1 (If b t f));