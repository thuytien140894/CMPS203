module Prettier 
    ( printPretty 
    ) where 

    import Error
    import GlobalState
    import Syntax 

    import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), (<+>))
    import qualified Text.PrettyPrint.ANSI.Leijen as PP

    class Pretty a where
        output :: a -> Doc
        
        printPretty :: a -> IO ()
        printPretty = PP.putDoc . output
    
    instance Pretty AExp where 
        output a = case a of 
            Num n      -> PP.int n 
            Var x      -> PP.text x
            Add a1 a2  -> output a1 <+> PP.text "+" <+> output a2
            Sub a1 a2  -> output a1 <+> PP.text "-" <+> output a2
            Mult a1 a2 -> output a1 <+> PP.text "*" <+> output a2
            Div a1 a2  -> output a1 <+> PP.text "/" <+> output a2

    instance Pretty BExp where 
        output b = case b of 
            Tru           -> PP.text "true"
            Fls           -> PP.text "false"
            Not b'        -> PP.text "!" <> output b'
            And b1 b2     -> output b1 <+> PP.text "&&" <+> output b2
            Or b1 b2      -> output b1 <+> PP.text "||" <+> output b2
            Equal a1 a2   -> output a1 <+> PP.text "=" <+> output a2
            Less a1 a2    -> output a1 <+> PP.text "<" <+> output a2 
            Greater a1 a2 -> output a1 <+> PP.text ">" <+> output a2 
    
    instance Pretty Stm where 
        output c = case c of 
            Skip -> PP.text "skip"
            Assign x a -> PP.text (x ++ " :=") <+> output a 
            Seq c1 c2  -> output c1 <> PP.semi <+> output c2
            If b c1 c2 -> PP.text "if" <+> output b 
                          <+> PP.text "then" <+> output c1 
                          <+> PP.text "else" <+> output c2
            While b c' -> PP.text "while" <+> output b 
                          <+> PP.text "do" <+> output c'
                
    instance Pretty State where 
        output s = PP.text (show s)

    instance Pretty Stack where 
        output (Stack [])           = PP.empty
        output (Stack ((c, s): cs)) = PP.angles (output c <> PP.comma <+> output s) 
                                      <> PP.linebreak <> PP.text "->" 
                                      <+> output (Stack cs)

    instance Pretty Error where
        output e = case e of 
            NotInScope x -> PP.text "Exception: Variable not in scope:" 
                            <+> PP.squotes (PP.text x)
            DivByZero a  -> PP.text "Exception: Division by zero:" 
                            <+> PP.squotes (output a)
            Stuck        -> PP.text "Done"
