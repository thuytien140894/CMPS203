module Error where 

    import Syntax

    -- evaluation errors
    data Error 
        = NotInScope String 
        | DivByZero AExp
        | Stuck