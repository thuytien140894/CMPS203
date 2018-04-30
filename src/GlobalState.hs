module GlobalState where 

    import Syntax 

    import Data.Map (Map, toList)
    import qualified Data.Map as Map

    -- a store for variables and their values
    newtype State = State (Map String Int)
        deriving (Eq)

    instance Show State where 
        show (State s) = show $ toList s

    -- create an empty state
    emptyState :: State
    emptyState = State Map.empty 

    -- look up the value for a variable
    lookUp :: State -> String -> Maybe Int 
    lookUp (State s) x = Map.lookup x s

    -- insert a new variable or update the value for the existing variable
    update :: State -> String -> Int -> State
    update (State s) x n = State $ Map.insert x n s

    -- stack trace to store all the evaluation steps
    newtype Stack = Stack [(Stm, State)]

    -- create an empty stack 
    emptyStack :: Stack 
    emptyStack = Stack []

    -- push the next evaluation step to the stack
    push :: Stack -> (Stm, State) -> Stack
    push (Stack s) e = Stack $ s ++ [e]