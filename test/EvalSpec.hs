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
          Just (Num 8)

      context "5 * 3" $
        it "should be 15" $
          eval (Num 5 `Mult` Num 3)
          `shouldBe`
          Just (Num 15)

      context "3 - 10" $
        it "should be -7" $
          eval (Num 3 `Add` Num (-10))
          `shouldBe`
          Just (Num (-7))

      context "-3 * -3" $
        it "should be 9" $
          eval (Num (-3) `Mult` Num (-3))
          `shouldBe`
          Just (Num 9)