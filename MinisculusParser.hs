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

    The following LL(1) Grammar is used.

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
-}

module Main where

import AST
import Control.Exception
import Data.Either
import MinisculusLexer
import ParseTree
import System.Environment
import System.IO

parse :: [Token] -> (ParseTree, [Token])

parse ts = ((ParseTree s), ts1) where
    (s,ts1) = prog ts

prog :: [Token] -> (Prog, [Token])
prog ts = ((R0 s), ts1) where
    (s,ts1) = stmt ts

stmt (T_If:ts) = ((R1 If e Then s1 Else s2), ts3) where
    (e,ts1) = expr ts
    (s1,ts2) = stmt $ _stmt1 ts1
    (s2,ts3) = stmt $ _stmt2 ts2
    _stmt1 (T_Then:ts) = ts
    _stmt1 ts = ts
    _stmt2 (T_Else:ts) = ts
    _stmt2 ts = ts
stmt (T_While:ts) = ((R2 While e Do s), ts2) where 
    (e,ts1) = expr ts 
    (s,ts2) = stmt $ _stmt1 ts1
    _stmt1 (T_Do:ts) = ts
    _stmt1 ts = ts
stmt (T_Input:(T_Identifier str:ts)) = ((R3 Input (Identifier str)), ts)
stmt (T_Identifier str:ts) = ((R4 (Identifier str) Assign e), ts1) where
    (e,ts1) = expr $ _stmt1 ts
    _stmt1 (T_Assign:ts) = ts
    _stmt1 ts = ts
stmt (T_Write:ts) = ((R5 Write e), ts1) where
    (e,ts1) = expr ts
stmt (T_Begin:ts) = ((R6 Begin l), ts1) where
    (l,ts1) = stmtlist ts 
stmt (t:ts) = (StmtError,ts)

stmtlist (T_End:ts) = ((R8 End), ts)
-- Look ahead by one to see if we're entering a statement
stmtlist (T_If:ts)              = doStmt (T_If:ts)
stmtlist (T_While:ts)           = doStmt (T_While:ts)
stmtlist (T_Input:ts)           = doStmt (T_Input:ts)
stmtlist (T_Identifier str:ts)  = doStmt (T_Identifier str:ts)
stmtlist (T_Write:ts)           = doStmt (T_Write:ts)
stmtlist (T_Begin:ts)           = doStmt (T_Begin:ts)
stmtlist ts = (StmtListError StmtError,ts)
doStmt ts = do
    let (s,ts1) = stmt ts;
    case s of
        StmtError   -> (StmtListError s, ts1)
        _           -> ((R7 s Semicolon l), ts2) where
            (l,ts2) = stmtlist $ _stmtlist1 ts1 
            _stmtlist1 (T_Semicolon:ts) = ts
            _stmtlist1 ts = ts

expr ts = ((R9 t me), ts2) where
    (t,ts1) = term ts
    (me,ts2) = moreExpr ts1

moreExpr (T_Add:ts) = ((R10 Add e), ts1) where
    (e,ts1) = expr ts
moreExpr (T_Sub:ts) = ((R11 Sub e), ts1) where
    (e,ts1) = expr ts
moreExpr ts = ((R12), ts)

term ts = ((R13 f mt), ts2) where
    (f,ts1) = factor ts
    (mt,ts2) = moreTerm ts1

moreTerm (T_Mul:ts) = ((R14 Mul t), ts1) where
    (t,ts1) = term ts
moreTerm (T_Div:ts) = ((R15 Div t), ts1) where
    (t,ts1) = term ts
moreTerm ts = ((R16), ts)

factor (T_LPar:ts) = ((R17 LPar e RPar), ts2) where
    (e,ts1) = expr ts
    ts2 = _factor1 ts1
    _factor1 (T_RPar:ts) = ts
    _factor1 ts = ts
factor (T_Identifier str:ts) = ((R18 (Identifier str)), ts)
factor (T_Num n:ts) = ((R19 (Num n)), ts)
factor (T_Sub:(T_Num n:ts)) = ((R20 Sub (Num n)), ts)

main = do
    args <- getArgs 
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
                error ("Parse Error on token " ++ show (head toks) ++
                    "\nNot Processed: " ++ show toks)

