module GlobalState where 

  import Syntax 

  import Data.Map (Map, toList)
  import qualified Data.Map as Map

  newtype State = State (Map String Int)
    deriving (Eq, Show)

  -- create an empty state
  emptyState :: State
  emptyState = State Map.empty

  -- look up the value for a variable
  lookUp :: State -> String -> Maybe Int 
  lookUp (State s) x = Map.lookup x s

  -- insert a new variable or update the value for the existing variable
  update :: State -> String -> Int -> State
  update (State s) x n = State $ Map.insert x n s