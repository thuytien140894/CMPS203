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

**2. How to build the program**

   Go to the program directory, then run the following command:  
   
        stack build 

**3. How to run tests on the program**

   Run the command:  
   
        stack test
        
   or 

        stack exec ARITH-exe 
