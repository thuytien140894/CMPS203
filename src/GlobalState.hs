{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module GlobalState where 

  import Syntax 

  import Data.Map (Map, toList)
  import qualified Data.Map as Map

  newtype State = State (Map String Int)
    deriving (Eq, Show)

  pattern EmptyState :: State 
  pattern EmptyState <- State (Map.null -> True) 
    where EmptyState = State Map.empty

  lookUp :: State -> String -> Maybe Int 
  lookUp (State s) x = Map.lookup x s

  update :: State -> String -> Int -> State
  update (State s) x n = State $ Map.insert x n s