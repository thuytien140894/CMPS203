module EvalSpec where

  import Test.Hspec
  import Syntax
  import Evaluator

  spec :: Spec
  spec = 
    describe "evaluates" $ 
      it "hello" $
        eval (Num 5 `Add` Num 3)
        `shouldBe`
        Just (Num 8)