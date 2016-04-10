module Main where

import Parser
import Expr

testSimpleAssignmemnt :: Bool
testSimpleAssignmemnt = parseString "x := 1" == Assign (Var "x") (Num 1)

testSmallProgram :: Bool
testSmallProgram = parseString expr == ast
    where
        expr = "read x; if y + 1 == x then write y else skip"
        ast = Colon
                (Read (Var "x"))
                (IfCond
                    (BinOp Eq (BinOp Add (Var "y") (Num 1)) (Var "x"))
                    (Write (Var "y"))
                    Skip)

main :: IO ()
main = do
    putStrLn $ "testSimpleAssignmemnt: " ++ show testSimpleAssignmemnt
    putStrLn $ "testSmallProgram: " ++ show testSmallProgram
