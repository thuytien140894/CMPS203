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

      context "(5 * 10) + 3" $
        it "should be 53" $
          eval (Num 5 `Mult` Num 10 `Add` Num 3)
          `shouldBe`
          Right (Num 53)
  
      context "((5 * 10) / 2) + 3" $
        it "should be 28" $
          eval ((Num 5 `Mult` Num 10 `Div` Num 2) `Add` Num 3)
          `shouldBe`
          Right (Num 28)

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

      context "-11 / 2" $
        it "should be -6" $
          eval (Num (-11) `Div` Num 2)
          `shouldBe`
          Right (Num (-6))

      context "11 / 2" $
        it "should be 5" $
          eval (Num 11 `Div` Num 2)
          `shouldBe`
          Right (Num 5)

      context "((-9 / -2) * (8 + 7) - 4" $
        it "should be 56" $
          eval ((Num (-9) `Div` Num (-2)) `Mult` (Num 8 `Add` Num 7) `Sub` Num 4)
          `shouldBe`
          Right (Num 56)

      context "(9 * 3) + (7 - 9)" $
        it "should be 25" $
          eval ((Num 9 `Mult` Num 3) `Add` (Num 7 `Sub` Num 9))
          `shouldBe`
          Right (Num 25)

      context "(10 / 3) + (4 + 9) / 0" $
        it "should be invalid" $
          eval ((Num 10 `Mult` Num 3) `Add` Num 2 `Add` Num 9 `Div` Num 0)
          `shouldBe`
          Left "Divide by 0"

      context "9 / 0" $
        it "should be invalid" $
          eval (Num 9 `Div` Num 0)
          `shouldBe`
          Left "Divide by 0"