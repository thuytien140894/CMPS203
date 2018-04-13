# ARITH
 
This program implemented an interpreter for the abstract syntax tree of ARITH. 
The AST of ARITH is defined as:

        t ::= Num n
              Add t t
              Sub t t
              Mult t t 
              Div t t 

To run this program, make sure that Haskell and the Stack build tool are installed 
on the system. 

**1. How to install Stack** 
    
   For Unix operating systems, run one of these commands:  

        curl -sSL https://get.haskellstack.org/ | sh  
         
   or   
   
        wget -qO- https://get.haskellstack.org/ | sh  

   If [homebrew](https://brew.sh/) is available, run:
   
        brew install haskell-stack  
        
   For Windows, download and install [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)  

**2. How to build and run the program**

   Go to the program directory, then run the following commands in the same order:  
   
        stack build  
        stack exec GTLC-exe  

**3. How to use the program**

   For integer integrals where n is an integer, type:
        
        Num n
        
   To add two integers, type:    
   
        Add (Num n) (Num m)
        
   To subtract two integers, type:    
   
        Sub (Num n) (Num m) 

   To multiply two integers, type:    
   
        Mult (Num n) (Num m)
   
   To divide two integers, type:    
   
        Div (Num n) (Num m)

   An operation can be performed between any two valid ARITH expressions. For instance, 
   one can add two expressions, each of which has its own operation, as follows:

        Add (Add (Num n) (Num m)) (Sub (Num j) (Num k)) 

   Enclose a group of operations in parentheses whenever necessary to enforce precedence.  
        
**4. How to exit the program**

        Type "exit" in the console

**5. How to run tests on the program**

   Run the command:  
   
        stack test
