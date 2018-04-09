module Syntax where 
  
  data Term 
    = Num Int
    | Add Term Term
    | Mult Term Term
    deriving (Eq, Show)


