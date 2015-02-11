{-
    Minisculus Recursive Descent Parser 
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Usage
    =====

    ./mcc file 

    Emits the stack code for the given Minisculus source to stdout. 
    Displays an error message if the source code is invalid.

    Grammar 
    ======

    The following LL(1) and Recursive Descent Grammar is used for
    Parsing. The `parse` function emits a Parse Tree that follows this
    grammar's form, which can be easily transformed into an AST.

    prog -> stmt.
    stmt -> IF expr THEN stmt ELSE stmt
                | WHILE expr DO stmt
                | INPUT ID
                | ID ASSIGN expr
                | WRITE expr
                | BEGIN stmtlist.
    stmtlist -> stmt semicolon stmtlist 
                | END.
    semicolon -> SEMICOLON.
    expr -> term moreexpr.
    moreexpr -> ADD expr 
                | SUB expr
                | .
    term -> factor moreterm.
    moreterm -> MUL term 
                | DIV term
                | .
    factor -> LPAR expr rpar 
                | ID
                | NUM
                | SUB NUM.
    rpar -> RPAR.

    AST Data Type
    =============

    The AST generated from the Parse Tree is of the form:

    data AST    = AST A_Prog
    data A_Prog = A_Prog A_Stmt
    data A_Stmt = A_IfThenElse A_Expr A_Stmt A_Stmt
                | A_While A_Expr A_Stmt
                | A_Input A_Identifier
                | A_Assign A_Identifier A_Expr
                | A_Write A_Expr
                | A_Block [A_Stmt]
    data A_Expr = A_Add A_Expr A_Expr 
                | A_Sub A_Expr A_Expr
                | A_Mul A_Expr A_Expr
                | A_Div A_Expr A_Expr
                | A_Var A_Identifier
                | A_Const A_Num 
    data A_Identifier = A_Identifier String
    data A_Num  = A_Num Int 

    Every AST is semantically equivalent to exactly one Parse Tree.
        
-}

module Main where

import Control.Exception
import Data.Either
import MinisculusAST
import MinisculusCodeGenerator
import MinisculusError
import MinisculusLexer
import MinisculusParser
import System.Environment
import System.IO

main = do
    args <- getArgs 
    if args == [] then
        error "Usage: ./MinisculusParser file"
    else do 
        let fn = head args
        file <- openFile fn ReadMode
        s <- hGetContents file
        let t = gettokens s
        case t of
            Left e  -> lexingError e 
            Right l -> do
                let (pt, toks) = parse $ tokenUnwrap l
                putStrLn $ generateCode 0 $ fromParseTree pt 

