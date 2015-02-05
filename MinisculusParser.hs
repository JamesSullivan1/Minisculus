{-
    Minisculus Parse Tree 
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Defines the ParseTree data structure for the Minisculus Parser, and
    a parsing function to take a list of Tokens and produce a pair
    consisting of the ParseTree built and the leftover tokens.

    A valid parse is a parse that results in an empty remaining token
    list; if this list is not empty, the input is not grammatically
    correct.

    This Parse Tree corresponds to the following LL(1) grammar: 

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

module MinisculusParser where

import MinisculusLexer

data ParseTree = ParseTree Prog
data Prog =     R0 Stmt
data Stmt =     R1 If Expr Then Stmt Else Stmt
            |   R2 While Expr Do Stmt
            |   R3 Input Identifier
            |   R4 Identifier Assign Expr
            |   R5 Write Expr
            |   R6 Begin StmtList
            |   StmtError
data StmtList = R7 Stmt Semicolon StmtList
            |   R8 End
            |   StmtListError Stmt
data Expr =     R9 Term MoreExpr
data MoreExpr = R10 Add Expr
            |   R11 Sub Expr
            |   R12
data Term =     R13 Factor MoreTerm
data MoreTerm = R14 Mul Term
            |   R15 Div Term
            |   R16
data Factor =   R17 LPar Expr RPar
            |   R18 Identifier
            |   R19 MinisculusParser.Num
            |   R20 Sub MinisculusParser.Num
data If = If
data Then = Then
data Else = Else
data While = While
data Do = Do
data Input = Input
data Identifier = Identifier String
data Num = Num Int
data Assign = Assign
data Write = Write
data Begin = Begin
data End = End
data Semicolon = Semicolon
data Add = Add
data Sub = Sub
data Mul = Mul
data Div = Div
data LPar = LPar
data RPar = RPar

parse :: [Token] -> (ParseTree, [Token])

parse ts = ((ParseTree s), ts1) where
    (s,ts1) = prog ts

prog :: [Token] -> (Prog, [Token])
prog ts = ((R0 s), ts1) where
    (s,ts1) = stmt ts

stmt :: [Token] -> (Stmt, [Token])
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

stmtlist :: [Token] -> (StmtList, [Token])
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

expr :: [Token] -> (Expr, [Token])
expr ts = ((R9 t me), ts2) where
    (t,ts1) = term ts
    (me,ts2) = moreExpr ts1

moreExpr :: [Token] -> (MoreExpr, [Token])
moreExpr (T_Add:ts) = ((R10 Add e), ts1) where
    (e,ts1) = expr ts
moreExpr (T_Sub:ts) = ((R11 Sub e), ts1) where
    (e,ts1) = expr ts
moreExpr ts = ((R12), ts)

term :: [Token] -> (Term, [Token])
term ts = ((R13 f mt), ts2) where
    (f,ts1) = factor ts
    (mt,ts2) = moreTerm ts1

moreTerm :: [Token] -> (MoreTerm, [Token])
moreTerm (T_Mul:ts) = ((R14 Mul t), ts1) where
    (t,ts1) = term ts
moreTerm (T_Div:ts) = ((R15 Div t), ts1) where
    (t,ts1) = term ts
moreTerm ts = ((R16), ts)

factor :: [Token] -> (Factor, [Token])
factor (T_LPar:ts) = ((R17 LPar e RPar), ts2) where
    (e,ts1) = expr ts
    ts2 = _factor1 ts1
    _factor1 (T_RPar:ts) = ts
    _factor1 ts = ts
factor (T_Identifier str:ts) = ((R18 (Identifier str)), ts)
factor (T_Num n:ts) = ((R19 (Num n)), ts)
factor (T_Sub:(T_Num n:ts)) = ((R20 Sub (Num n)), ts)

