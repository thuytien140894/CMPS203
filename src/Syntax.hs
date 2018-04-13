module Syntax where 
  
  data Term 
    = Num Int
    | Add Term Term
    | Mult Term Term
    | Sub Term Term 
    | Div Term Term
    deriving (Eq, Show)


