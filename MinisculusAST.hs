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
            | A_Begin A_StmtList
data A_StmtList = A_Semicolon A_Stmt A_StmtList
            | A_EndSL
data A_Expr = A_Expr A_Term MoreA_Expr
data MoreA_Expr = A_Add A_Expr
            | A_Sub A_Expr
            | A_EndME
data A_Term = A_Term A_Factor MoreA_Term
data A_Factor = A_LPar A_Expr A_RPar
            | Var A_Identifier
            | Const MinisculusAST.A_Num
data MoreA_Term = A_Mul A_Term
            | A_Div A_Term
            | A_EndMT
data A_RPar = A_RPar 
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
    A_Begin (fromStmtList l)

fromStmtList (R7 s Semicolon l) =
    A_Semicolon (fromStmt s) (fromStmtList l)
fromStmtList (R8 End) =
    A_EndSL 

fromExpr (R9 t me) =
    A_Expr (fromTerm t) (fromMoreExpr me)

fromMoreExpr (R10 Add e) =
    A_Add (fromExpr e)
fromMoreExpr (R11 Sub e) =
    A_Sub (fromExpr e)
fromMoreExpr (R12) =
    A_EndME

fromTerm (R13 f mt) =
    A_Term (fromFactor f) (fromMoreTerm mt)

fromMoreTerm (R14 Mul t) =
    A_Mul (fromTerm t)
fromMoreTerm (R15 Div t) =
    A_Div (fromTerm t)
fromMoreTerm (R16) =
    A_EndMT

fromFactor (R17 LPar e RPar) =
    A_LPar (fromExpr e) A_RPar
fromFactor (R18 (Identifier s)) =
    Var (A_Identifier s)
fromFactor (R19 (MinisculusParser.Num n)) =
    Const (A_Num n)
fromFactor (R20 Sub (MinisculusParser.Num n)) =
    Const (A_Num (-1 * n))

instance Show AST where
    show (AST a) = "PROGRAM (\n" ++ show a ++ ") ENDPROGRAM"
instance Show A_Prog where
    show (A_Prog a) = show a
instance Show A_Stmt where
    show (A_IfThenElse e s1 s2) = "IF " ++ show e ++ 
        " THEN (\n" ++ show s1 ++ ") " ++
        "ELSE (\n" ++ show s2 ++ "\n) ENDELSE"
    show (A_While e s) = "WHILE " ++ show e ++ " DO (\n" ++ show s 
        ++ "\n) ENDWHILE"
    show (A_Input i) = "INPUT (" ++ show i ++ ")"
    show (A_Assign i e) = "(" ++
        show i ++ " := " ++ show e ++ ")"
    show (A_Write e) = "(" ++
        "WRITE " ++ show e ++ ")"
    show (A_Begin l) = "BEGIN [\n" ++ show l ++ "] END\n"
instance Show A_StmtList where
    show (A_Semicolon s l) = "" ++
        show s ++ ";" ++ maybeNewLine l ++ show l where
        maybeNewLine (A_Semicolon s l) = "\n"
        maybeNewLine (A_EndSL) = ""
    show (A_EndSL) = "\n"
instance Show A_Expr where
    show (A_Expr t me) = show t ++ show me
instance Show A_Term where
    show (A_Term f mt) = show f ++ show mt
instance Show MoreA_Expr where
    show (A_Add t) = " + " ++ show t
    show (A_Sub t) = " - " ++ show t
    show (A_EndME) = ""
instance Show MoreA_Term where
    show (A_Mul t) = " * " ++ show t
    show (A_Div t) = " / " ++ show t
    show (A_EndMT) = ""
instance Show A_Factor where
    show (A_LPar e r) = "(" ++ show e ++ ")"
    show (Var i) = "(" ++ show i ++ ")"
    show (Const n) = "(" ++ show n ++ ")"
instance Show A_Identifier where
    show (A_Identifier s) = "Var " ++ show s
instance Show A_Num where
    show (A_Num n) = "Const " ++ show n

