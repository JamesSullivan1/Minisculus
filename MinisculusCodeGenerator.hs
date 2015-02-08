{-
    Minisculus Stack Code Generator 
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Implements a stack code generator for a Minisculus AST.

 -}


module MinisculusCodeGenerator where

import MinisculusAST
import Data.Char

generateCode :: Int -> AST -> String
generateCode n (AST a) = generateCodeProg n a

generateCodeProg :: Int -> A_Prog -> String
generateCodeProg n (A_Prog s) = fst $ generateCodeStmt n s

generateCodeStmt :: Int -> A_Stmt -> (String, Int)
generateCodeStmt n (A_IfThenElse e s1 s2) =
    (generateCodeExpr e ++ "cJUMP label"++(show n)++"\n"
        ++ c1
        ++ "JUMP label"++(show (n+1))++"\n"
        ++ "label"++(show n)++":\n"
        ++ c2
        ++ "label"++(show (n+1))++":\n",m) where
    (c1, n1) = generateCodeStmt (n+2) s1
    (c2, m)  = generateCodeStmt (n1) s2
generateCodeStmt n (A_While e s) =
    ("label"++(show n)++":\n" 
        ++ generateCodeExpr e 
        ++ "cJUMP label"++(show (n+1))++"\n"
        ++ c1
        ++ "JUMP label"++(show n)++"\n"
        ++ "label"++(show (n+1))++":\n",m) where
    (c1, m) = generateCodeStmt (n+2) s
generateCodeStmt n (A_Input (A_Identifier v)) =
    ("READ "++ v ++"\n",n)
generateCodeStmt n (A_Assign (A_Identifier v) e) =
    (generateCodeExpr e ++ "LOAD " ++ v ++ "\n",n)
generateCodeStmt n (A_Write e) =
    (generateCodeExpr e ++ "PRINT\n",n)
generateCodeStmt n (A_Block (s:rest)) =
    (c1 ++ cr, m) where
    (c1, n1) = generateCodeStmt n s
    (cr, m)  = generateCodeStmt n1 (A_Block rest)
generateCodeStmt n (A_Block []) = ("",n)

generateCodeExpr :: A_Expr -> String
generateCodeExpr (A_Add e1 e2) =
    generateCodeExpr e1 ++ generateCodeExpr e2 ++ "OP2 +\n" 
generateCodeExpr (A_Sub e1 e2) =
    generateCodeExpr e1 ++ generateCodeExpr e2 ++ "OP2 -\n" 
generateCodeExpr (A_Mul e1 e2) =
    generateCodeExpr e1 ++ generateCodeExpr e2 ++ "OP2 *\n" 
generateCodeExpr (A_Div e1 e2) =
    generateCodeExpr e1 ++ generateCodeExpr e2 ++ "OP2 /\n" 
generateCodeExpr (A_Var (A_Identifier i)) = 
    "rPUSH " ++ i ++ "\n"
generateCodeExpr (A_Const c) =
    tidy $ "cPUSH " ++ show c ++ "\n" where
    tidy s = (filter (/='"') s) 
