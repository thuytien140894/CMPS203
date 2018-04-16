module GlobalState where 

  import Syntax 

  import Data.Map (Map)
  import qualified Data.Map as Map

  newtype State = State (Map String Int)

  lookUp :: State -> String -> Maybe Int 
  lookUp (State s) x = Map.lookup x s

  update :: State -> String -> Int -> State
  update (State s) x n = State $ Map.insert x n s