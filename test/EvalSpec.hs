module EvalSpec where

  import Test.Hspec
  import Syntax
  import Evaluator

  spec :: Spec
  spec = 
    describe "evaluates" $ do
      context "5 + 3" $
        it "should be 8" $
          eval (Num 5 `Add` Num 3)
          `shouldBe`
          Right (Num 8)

      context "5 * 3" $
        it "should be 15" $
          eval (Num 5 `Mult` Num 3)
          `shouldBe`
          Right (Num 15)

      context "3 - 10" $
        it "should be -7" $
          eval (Num 3 `Sub` Num 10)
          `shouldBe`
          Right (Num (-7))

      context "9 / 3" $
        it "should be 3" $
          eval (Num 9 `Div` Num 3)
          `shouldBe`
          Right (Num 3)

      context "9 / 0" $
        it "should be invalid" $
          eval (Num 9 `Div` Num 0)
          `shouldBe`
          Left "Divide by 0"