module Syntax where 
  
  data AExp 
    = Num Int 
    | Var String 
    | Add AExp AExp
    | Sub AExp AExp
    | Mult AExp AExp
    deriving (Eq, Show)

  data BExp 
    = Tru
    | Fls 
    | Equal AExp AExp
    | Less AExp AExp
    | Not BExp
    | And BExp BExp
    | Or BExp BExp
    deriving (Eq, Show)

  data Stm 
    = Skip 
    | Assign String AExp
    | Seq Stm Stm
    | If BExp Stm Stm
    | While BExp Stm
    deriving (Eq, Show)