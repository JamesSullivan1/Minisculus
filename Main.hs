{-
    Minisculus Recursive Descent Parser 
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Usage
    =====

    ./MinisculusParser file 

    Print the AST of the given Minisculus source to stdout, or display
    an error message on a lexical/grammatical failure.

    Grammar 
    ======

    The following LL(1) Grammar is used for Parsing. The `parse`
    function emits a Parse Tree that follows this grammar's form, which
    can be easily transformed into an AST.

    prog -> stmt.
    stmt -> IF expr THEN stmt ELSE stmt
                | WHILE expr DO stmt
                | INPUT ID
                | ID ASSIGN expr
                | WRITE expr
                | BEGIN stmtlist.
    stmtlist -> stmt SEMICOLON stmtlist 
                | END.
    expr -> term moreexpr.
    moreexpr -> ADD expr 
                | SUB expr
                | .
    term -> factor moreterm.
    moreterm -> MUL term 
                | DIV term
                | .
    factor -> LPAR expr RPAR
                | ID
                | NUM
                | SUB NUM.

    AST Data Type
    =============

    The AST generated from the Parse Tree is of the form:

    data AST        = AST A_Prog
    data A_Prog     = A_Prog A_Stmt
    data A_Stmt     = A_IfThenElse A_Expr A_Stmt A_Stmt
                    | A_While A_Expr A_Stmt
                    | A_Input A_Identifier
                    | A_Assign A_Identifier A_Expr
                    | A_Write A_Expr
                    | A_Begin A_StmtList
    data A_StmtList = A_Semicolon A_Stmt A_StmtList
                    | A_EndSL
    data A_Expr     = A_Expr A_Term MoreA_Expr
    data MoreA_Expr = A_Add A_Expr
                    | A_Sub A_Expr
                    | A_EndME
    data A_Term     = A_Term A_Factor MoreA_Term
    data A_Factor   = A_LPar A_Expr A_RPar
                    | Var A_Identifier
                    | Const AST.A_Num
    data MoreA_Term = A_Mul A_Term
                    | A_Div A_Term
                    | A_EndMT
    data A_RPar     = A_RPar 
    data A_Identifier = A_Identifier String
    data A_Num      = A_Num Int 

    Every AST is semantically equivalent to exactly one Parse Tree.
        
-}

module Main where

import Control.Exception
import Data.Either
import MinisculusAST
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
            Left e  -> error e 
            Right l -> do
                let (pt, toks) = parse l
                if toks == [] then
                    print (fromParseTree pt)
                else 
                    error ("Parse Error on token " ++ show (head toks) 
                        ++ "\nNot Processed: " ++ show toks)

