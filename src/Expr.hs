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
          | Num Int
          | BinOp Op Expr Expr deriving (Eq, Show)

--TODO: fix assignment. It is sooo fucked up
data Stmt = Skip
          | Assign Expr Expr
          | Colon Stmt Stmt
          | Write Expr
          | Read Expr
          | WhileLoop Expr Stmt
          | IfCond Expr Stmt Stmt deriving (Eq, Show)
