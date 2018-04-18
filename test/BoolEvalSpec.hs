module BoolEvalSpec where

  import Syntax
  import Evaluator (bEval)
  import GlobalState

  import Test.Hspec

  spec :: Spec
  spec = 
    describe "evaluates" $ do
      context "true" $
        it "should be true" $
          bEval Tru EmptyState
          `shouldBe`
          Right Tru

      context "false" $
        it "should be false" $
          bEval Fls EmptyState
          `shouldBe`
          Right Fls

      context "5 == 3" $
        it "should be false" $
          bEval (Num 5 `Equal` Num 3) EmptyState
          `shouldBe`
          Right Fls

      context "3 < 10" $
        it "should be true" $
          bEval (Num 3 `Less` Num 10) EmptyState
          `shouldBe`
          Right Tru

      context "!(10 == (3 + 7)" $
        it "should be false" $
          let b = Num 10 `Equal` (Num 3 `Add` Num 7)
          in bEval (Not b) EmptyState
          `shouldBe`
          Right Fls

      context "(x == 3) && (y < 4) and [x=3,y=5]" $
        it "should be false" $ 
          let s  = update EmptyState "x" 3
              s' = update s "y" 5
          in bEval ((Var "x" `Equal` Num 3) `And` (Var "y" `Less` Num 4)) s'
          `shouldBe`
          Right Fls

      context "(x == 3) || (y < 4) and [x=3,y=5]" $
        it "should be true" $
          let s  = update EmptyState "x" 3
              s' = update s "y" 5
          in bEval ((Var "x" `Equal` Num 3) `Or` (Var "y" `Less` Num 4)) s'
          `shouldBe`
          Right Tru