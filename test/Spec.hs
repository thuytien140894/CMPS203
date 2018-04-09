import Test.Hspec
import Evaluator

main :: IO ()
main = hspec $ 
  describe "evaluates" $ 
    it "prints out \"Hello World!\"" 
      someFunc 
