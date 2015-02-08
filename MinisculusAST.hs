{-
    Minisculus AST 
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Defines the AST data structure for the Minisculus Language.

    Includes a function fromParseTree :: ParseTree -> AST that
    transforms a completed parse tree to an AST.

 -}

module MinisculusAST where

import MinisculusParser

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

fromParseTree :: ParseTree -> AST
fromParseTree (ParseTree p) = AST (fromProg p)

fromProg (R0 s) = A_Prog (fromStmt s)

fromStmt (R1 If e Then s1 Else s2) = 
    A_IfThenElse (fromExpr e) (fromStmt s1) (fromStmt s2)
fromStmt (R2 While e Do s) = 
    A_While (fromExpr e) (fromStmt s)
fromStmt (R3 Input (Identifier s)) = 
    A_Input (A_Identifier s)
fromStmt (R4 (Identifier s) Assign e) = 
    A_Assign (A_Identifier s) (fromExpr e)
fromStmt (R5 Write e) =
    A_Write (fromExpr e)
fromStmt (R6 Begin l) =
    A_Block (fromStmtList l)

fromStmtList (R7 s Semicolon l) =
    (fromStmt s):(fromStmtList l)
fromStmtList (R8 End) =
    []

fromExpr (R9 t (R10 Add e)) =
    A_Add (fromTerm t) (fromExpr e)
fromExpr (R9 t (R11 Sub e)) = 
    A_Sub (fromTerm t) (fromExpr e)
fromExpr (R9 t (R12)) =
    fromTerm t

fromTerm (R13 f (R14 Mul t)) =
    A_Mul (fromFactor f) (fromTerm t)
fromTerm (R13 f (R15 Div t)) =
    A_Div (fromFactor f) (fromTerm t)
fromTerm (R13 f (R16)) =
    fromFactor f

fromFactor (R17 LPar e RPar) =
    fromExpr e
fromFactor (R18 (Identifier s)) =
    A_Var (A_Identifier s)
fromFactor (R19 (MinisculusParser.Num n)) =
    A_Const (A_Num n)
fromFactor (R20 Sub (MinisculusParser.Num n)) =
    A_Const (A_Num (-1 * n))

instance Show AST where
    show (AST a) = "PROGRAM (\n" ++ show a ++ ") ENDPROGRAM"
instance Show A_Prog where
    show (A_Prog a) = show a
instance Show A_Stmt where
    show (A_IfThenElse e s1 s2) = "IF " ++ show e ++ 
        " THEN (\n" ++ show s1 ++ ") " ++
        "ELSE (\n" ++ show s2 ++ ") ENDELSE;\n"
    show (A_While e s) = "WHILE " ++ show e ++ " DO (\n" ++ show s 
        ++ ") ENDWHILE;\n"
    show (A_Input i) = "INPUT (" ++ show i ++ ");\n"
    show (A_Assign i e) = "(" ++
        show i ++ " := " ++ show e ++ ");\n"
    show (A_Write e) = "(" ++
        "WRITE " ++ show e ++ ");\n"
    show (A_Block l) = "BEGIN [\n" ++ foldr1 (++) (map show l) ++ 
        "] END;\n"
instance Show A_Expr where
    show (A_Add e t) = show e ++ " + " ++ show t
    show (A_Sub e t) = show e ++ " - " ++ show t
    show (A_Mul e t) = show e ++ " * " ++ show t
    show (A_Div e t) = show e ++ " / " ++ show t
    show (A_Var i) = "(" ++ show i ++ ")"
    show (A_Const n) = "(" ++ show n ++ ")"
instance Show A_Identifier where
    show (A_Identifier s) = "Var " ++ show s
instance Show A_Num where
    show (A_Num n) = "Const " ++ show n

