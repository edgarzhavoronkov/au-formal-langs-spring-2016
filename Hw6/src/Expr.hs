module Expr where

type Id = String

data Op = Add
        | Sub
        | Mul
        | Div
        | Mod
        | Eq
        | Neq
        | Gt
        | Geq
        | Lt
        | Leq
        | And
        | Or deriving (Eq, Show)

data Expr = Var Id
          | Num Integer
          | BinOp Op Expr Expr deriving (Eq, Show)

--TODO: fix assignment. It is sooo fucked up
data Stmt = Skip
          | Assign Expr Expr
          | Colon Stmt Stmt
          | Write Expr
          | Read Expr
          | WhileLoop Expr Stmt
          | IfCond Expr Stmt Stmt deriving (Eq)

instance Show Stmt where
    show = helper ""
        where
            helper :: String -> Stmt -> String
            helper indent Skip = indent ++ "Skip" ++ "\n"
            helper indent (Assign e1 e2) = indent ++ "Assign " ++ show e1 ++ show e2 ++ "\n"
            helper indent (Colon s1 s2) = indent ++ "Colon " ++ "\n" ++ helper ('\t':indent) s1 ++ helper ('\t':indent) s2 ++ "\n"
            helper indent (Write e) = indent ++ "Write " ++ show e ++ "\n"
            helper indent (Read e) = indent ++ "Read " ++ show e ++ "\n"
            helper indent (WhileLoop e s) = indent ++ "While " ++ "\n" ++ ('\t':indent) ++ show e ++ "\n" ++ helper ('\t':indent) s
            helper indent (IfCond e s1 s2) = indent ++ "If " ++ "\n" ++ ('\t':indent) ++ show e ++ "\n" ++ indent ++ "Then" ++ "\n" ++ helper ('\t':indent) s1 ++ indent ++ "Else" ++ "\n" ++ helper ('\t':indent) s2
