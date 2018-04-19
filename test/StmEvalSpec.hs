module StmEvalSpec where

  import Syntax
  import Evaluator (cEval)
  import GlobalState

  import Test.Hspec
  import Data.Map (fromList)

  spec :: Spec
  spec = 
    describe "evaluates" $ do
      context "skip []" $
        it "should be []" $
          cEval Skip EmptyState
          `shouldBe`
          Right EmptyState

      context "x=3;x=7" $
        it "should be [x=7]" $
          let c1 = Assign "x" (Num 3)
              c2 = Assign "x" (Num 7)
          in cEval (Seq c1 c2) EmptyState
          `shouldBe`
          Right (State (fromList [("x",7)]))

      context "x=7;if false then x=x+1 else x=x+2" $
        it "should be [x=9]" $
          let c1 = Assign "x" (Num 7)
              t  = "x" `Assign` (Var "x" `Add` Num 1)
              f  = "x" `Assign` (Var "x" `Add` Num 2)
          in cEval (Seq c1 (If Fls t f)) EmptyState
          `shouldBe`
          Right (State (fromList [("x",9)]))

      context "x=1;x=x+3" $
        it "should be [x=4]" $
          let c1 = Assign "x" (Num 1)
              c2 = Assign "x" (Var "x" `Add` Num 3)
          in cEval (Seq c1 c2) EmptyState
          `shouldBe`
          Right (State (fromList [("x",4)]))

      context "x=1;while x < 5 do x=x+1" $
        it "should be [x=5]" $
          let c1 = Assign "x" (Num 1)
              b  = Var "x" `Less` Num 5
              c2 = Assign "x" (Var "x" `Add` Num 1)
          in cEval (Seq c1 (While b c2)) EmptyState
          `shouldBe`
          Right (State (fromList [("x",5)]))

      context "for (x=0; x < 5; x=x+1)" $
        it "should be [x=5]" $
          let c1 = Assign "x" (Num 0)
              b  = Var "x" `Less` Num 5
              c2 = Assign "x" (Var "x" `Add` Num 1)
              c3 = Seq c2 Skip
          in cEval (For c1 b c3) EmptyState
          `shouldBe`
          Right (State (fromList [("x",5)]))

      context "y=1;for (x=0; x < 5; x=x+1)" $
        it "should be [x=5]" $
          let c0 = Assign "y" (Num 1)
              c1 = Assign "x" (Num 0)
              b  = Var "x" `Less` Num 5
              c2 = Assign "x" (Var "x" `Add` Num 1)
              c3 = Assign "y" (Var "y" `Mult` Num 2)
              c4 = Seq c2 c3
          in cEval (Seq c0 (For c1 b c4)) EmptyState
          `shouldBe`
          Right (State (fromList [("x",5),("y",32)]))