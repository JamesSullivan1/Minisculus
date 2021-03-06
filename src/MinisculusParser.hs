{-
    Minisculus Parse Tree 
    ================
    James Sullivan
    <sullivan.james.f@gmail.com>

    Defines the ParseTree data structure for the Minisculus Parser, and
    a parsing function to take a list of Tokens and produce a pair
    consisting of the ParseTree built and the leftover tokens.

    A valid parse is a parse that results in an empty remaining token
    list; if this list is not empty, the input is not grammatically
    correct.

    This Parse Tree corresponds to the following Recursive Descent, LL(1) 
    form grammar: 

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


-}

module MinisculusParser where

import MinisculusError
import MinisculusLexer
import MinisculusPos

data ParseTree = ParseTree Prog
data Prog   =   R0 Stmt
data Stmt   =   R1 If Expr Then Stmt Else Stmt
            |   R2 While Expr Do Stmt
            |   R3 Input Identifier
            |   R4 Identifier Assign Expr
            |   R5 Write Expr
            |   R6 Begin StmtList
data StmtList = R7 Stmt SSemicol StmtList
            |   R8 End
data SSemicol = R9 Semicolon
data Expr   =   R10 Term MoreExpr
data MoreExpr = R11 Add Expr
            |   R12 Sub Expr
            |   R13
data Term =     R14 Factor MoreTerm
data MoreTerm = R15 Mul Term
            |   R16 Div Term
            |   R17
data Factor =   R18 LPar Expr SRPar
            |   R19 Identifier
            |   R20 MinisculusParser.Num
            |   R21 Sub MinisculusParser.Num
data SRPar  =   R22 RPar
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

parse :: [(Pos,TokenClass)] -> (ParseTree, [(Pos,TokenClass)])
parse [] = error "Parsing Error - Empty token list"
parse ts = ((ParseTree s), ts1) where
    (s,ts1) = prog ts

prog :: [(Pos,TokenClass)] -> (Prog, [(Pos,TokenClass)])
prog ts = ((R0 s), ts1) where
    (s,ts1) = stmt ts

stmt :: [(Pos,TokenClass)] -> (Stmt, [(Pos,TokenClass)])
stmt ((_,T_If):ts) =
    ((R1 If e Then s1 Else s2), ts3) where
    (e,ts1) = expr ts
    (s1,ts2) = stmt $ _stmt1 ts1
    (s2,ts3) = stmt $ _stmt2 ts2
    _stmt1 ((_,T_Then):ts) = ts
    _stmt1 ts = ts
    _stmt2 ((_,T_Else):ts) = ts
    _stmt2 ts = ts
stmt ((_,T_While):ts) = 
    ((R2 While e Do s), ts2) where 
    (e,ts1) = expr ts 
    (s,ts2) = stmt $ _stmt1 ts1
    _stmt1 ((_,T_Do):ts) = ts
    _stmt1 ts = ts
stmt ((_,T_Input):((_,T_Identifier str):ts)) = 
    ((R3 Input (Identifier str)), ts)
stmt ((_,T_Identifier str):(_,T_Assign):ts) = 
    ((R4 (Identifier str) Assign e), ts1) where
    (e,ts1) = expr ts
stmt ((_,T_Write):ts) = 
    ((R5 Write e), ts1) where
    (e,ts1) = expr ts
stmt ((_,T_Begin):ts) = 
    ((R6 Begin l), ts1) where
    (l,ts1) = stmtlist ts 
stmt ((p,t):ts) =
    error $ parseError p t stmtExpected

stmtlist :: [(Pos,TokenClass)] -> (StmtList, [(Pos,TokenClass)])
stmtlist ((_,T_End):ts) = 
    ((R8 End), ts)
-- Look ahead by one to see if we're entering a statement
stmtlist ((p,T_If):ts)              = doStmt ((p,T_If):ts)
stmtlist ((p,T_While):ts)           = doStmt ((p,T_While):ts)
stmtlist ((p,T_Input):ts)           = doStmt ((p,T_Input):ts)
stmtlist ((p,T_Identifier str):ts)  = doStmt ((p,T_Identifier str):ts)
stmtlist ((p,T_Write):ts)           = doStmt ((p,T_Write):ts)
stmtlist ((p,T_Begin):ts)           = doStmt ((p,T_Begin):ts)
stmtlist ((p,t):ts) = 
    error $ parseError p t stmtListExpected 
doStmt ts = 
    ((R7 s sem l), ts3) where
    (s,ts1) = stmt ts
    (sem,ts2) = semicolon ts1
    (l,ts3) = stmtlist ts2 

semicolon :: [(Pos,TokenClass)] -> (SSemicol, [(Pos,TokenClass)])
semicolon ((_, T_Semicolon):ts) = 
    ((R9 Semicolon), ts)
semicolon ((p,t):ts) =
    error $ parseError p t semicolonExpected

expr :: [(Pos,TokenClass)] -> (Expr, [(Pos,TokenClass)])
expr ts = 
    ((R10 t me), ts2) where
    (t,ts1) = term ts
    (me,ts2) = moreExpr ts1

moreExpr :: [(Pos,TokenClass)] -> (MoreExpr, [(Pos,TokenClass)])
moreExpr ((_,T_Add):ts) = 
    ((R11 Add e), ts1) where
    (e,ts1) = expr ts
moreExpr ((_,T_Sub):ts) = 
    ((R12 Sub e), ts1) where
    (e,ts1) = expr ts
moreExpr ts = 
    ((R13), ts)

term :: [(Pos,TokenClass)] -> (Term, [(Pos,TokenClass)])
term ts = 
    ((R14 f mt), ts2) where
    (f,ts1) = factor ts
    (mt,ts2) = moreTerm ts1

moreTerm :: [(Pos,TokenClass)] -> (MoreTerm, [(Pos,TokenClass)])
moreTerm ((_,T_Mul):ts) = 
    ((R15 Mul t), ts1) where
    (t,ts1) = term ts
moreTerm ((_,T_Div):ts) = 
    ((R16 Div t), ts1) where
    (t,ts1) = term ts
moreTerm ts = 
    ((R17), ts)

factor :: [(Pos,TokenClass)] -> (Factor, [(Pos,TokenClass)])
factor ((_,T_LPar):ts) = 
    ((R18 LPar e srp), ts2) where
    (e,ts1) = expr ts
    (srp,ts2) = rPar ts1
factor ((_,T_Identifier str):ts) = 
    ((R19 (Identifier str)), ts)
factor ((_,T_Num n):ts) = 
    ((R20 (Num n)), ts)
factor ((_,T_Sub):((_,T_Num n):ts)) = 
    ((R21 Sub (Num n)), ts)
factor ((p,t):ts) =
    error $ parseError p t factorExpected

rPar :: [(Pos,TokenClass)] -> (SRPar, [(Pos,TokenClass)])
rPar ((_, T_RPar):ts) = 
    ((R22 RPar), ts)
rPar ((p,t):ts) =
    error $ parseError p t rParExpected


{- Error messages detailing the set of acceptable next symbols for each
 - given state. IE, the first sets of each state.
 -}
stmtExpected        = "if, while, input, [identifier], write, or begin"
progExpected        = stmtExpected
parseExpected       = progExpected
factorExpected      = "'(', [identifier], or [number]"
termExpected        = factorExpected
exprExpected        = termExpected
stmtListExpected    = "end, " ++ stmtExpected
semicolonExpected   = "';'"
rParExpected        = "')'"
moreExprExpected    = "'+', '-', " ++ termExpected
moreTermExpected    = "'*', '/', " ++ factorExpected

parseError p s expected = parsingError p msg where
    msg = "unexpected token '" ++ show s ++ "' (expected " ++
        expected ++ ")"

