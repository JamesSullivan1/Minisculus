module ParseTree where

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
            |   R19 ParseTree.Num
            |   R20 Sub ParseTree.Num
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

