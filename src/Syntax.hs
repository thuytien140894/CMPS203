module Syntax where 
  
    -- arithmetic expressions
    data AExp 
        = Num Int 
        | Var String 
        | Add AExp AExp
        | Sub AExp AExp
        | Mult AExp AExp
        | Div AExp AExp
        deriving (Eq, Show)

    -- boolean expressions
    data BExp 
        = Tru
        | Fls 
        | Equal AExp AExp
        | Less AExp AExp
        | Greater AExp AExp
        | Not BExp
        | And BExp BExp
        | Or BExp BExp
        deriving (Eq, Show)

    -- commands
    data Stm 
        = Skip 
        | Assign String AExp
        | Seq Stm Stm
        | If BExp Stm Stm
        | While BExp Stm
        deriving (Eq, Show)