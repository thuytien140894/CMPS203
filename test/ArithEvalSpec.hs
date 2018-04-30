module ArithEvalSpec where

    import Syntax
    import Evaluator (aEval)
    import GlobalState

    import Test.Hspec

    spec :: Spec
    spec = 
        describe "evaluates" $ do
            context "5" $
                it "should be 5" $
                aEval (Num 5) emptyState
                `shouldBe`
                Right (Num 5)

            context "5 + 3" $
                it "should be 8" $
                aEval (Num 5 `Add` Num 3) emptyState
                `shouldBe`
                Right (Num 8)

            context "5 * 3" $
                it "should be 15" $
                aEval (Num 5 `Mult` Num 3) emptyState
                `shouldBe`
                Right (Num 15)

            context "3 - 10" $
                it "should be -7" $
                aEval (Num 3 `Sub` Num 10) emptyState
                `shouldBe`
                Right (Num (-7))

            context "x and []" $
                it "should be free variable" $
                aEval (Var "x") emptyState
                `shouldBe`
                Left "Variable not in scope: x"

            context "x and [x=3]" $
                it "should be 3" $ 
                let s = update emptyState "x" 3
                in aEval (Var "x") s
                `shouldBe`
                Right (Num 3)

            context "x + 3 and [x=3]" $
                it "should be 6" $
                let s = update emptyState "x" 3
                in aEval (Var "x" `Add` Num 3) s
                `shouldBe`
                Right (Num 6)