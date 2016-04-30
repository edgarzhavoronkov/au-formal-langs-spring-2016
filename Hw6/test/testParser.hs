module Main where

import Parser
import Expr

testSimpleAssignmemnt :: Bool
testSimpleAssignmemnt = parseString "x := 1" == Assign (Var "x") (Num 1)

testSmallProgram :: Bool
testSmallProgram = parseString expr == ast
    where
        expr = "read x; if y + 1 == x then write y else skip fi"
        ast = Colon
                (Read (Var "x"))
                (IfCond
                    (BinOp Eq (BinOp Add (Var "y") (Num 1)) (Var "x"))
                    (Write (Var "y"))
                    Skip)

--test optimizations
testAddZeroRight :: Bool
testAddZeroRight = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := y + 0"
        expected = Assign (Var "x") (Var "y")

testAddZeroLeft :: Bool
testAddZeroLeft = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := 0 + y"
        expected = Assign (Var "x") (Var "y")

testMultUnitRight :: Bool
testMultUnitRight = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := y * 1"
        expected = Assign (Var "x") (Var "y")

testMultUnitLeft :: Bool
testMultUnitLeft = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := 1 * y"
        expected = Assign (Var "x") (Var "y")

testMultZeroRight :: Bool
testMultZeroRight = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := y * 0"
        expected = Assign (Var "x") (Num 0)

testMultZeroLeft :: Bool
testMultZeroLeft = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := 0 * y"
        expected = Assign (Var "x") (Num 0)

testDivUnit :: Bool
testDivUnit = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := y / 1"
        expected = Assign (Var "x") (Var "y")

testModUnit :: Bool
testModUnit = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "x := y % 1"
        expected = Assign (Var "x") (Num 0)

--testing evaluations
testEval :: Bool
testEval = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "write (1 + 2) * 4 + 30"
        expected = Write (Num 42)

testComplexEval :: Bool
testComplexEval = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "y := 30 + (x + (4 * 3))"
        expected = Assign (Var "y") (BinOp Add (Num 42) (Var "x"))

--testing logic ops
testLogicalOr :: Bool
testLogicalOr = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "y := 1 || (4 / 0)"
        expected = Assign (Var "y") (Num 1)

testLogicalAnd :: Bool
testLogicalAnd = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "y := 0 && (4 / 0)"
        expected = Assign (Var "y") (Num 0)

--remove while loop
testRemoveWhileLoop :: Bool
testRemoveWhileLoop = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "while (x + 3 * 4) && (3 + 5 - 2 - 6) do y := (2 / 0) od"
        expected = Skip

--remove if statement
testRemoveIfStatement :: Bool
testRemoveIfStatement = expected == actual
    where
        actual = optimize $ parseString expr
        expr = "if (x + 3 * 4) || ((3 + 6) - (3 + 5)) then write x else write (1 / 0) fi"
        expected = Write (Var "x")

main :: IO ()
main = do
    putStrLn $ "testSimpleAssignmemnt: " ++ show testSimpleAssignmemnt
    putStrLn $ "testSmallProgram: " ++ show testSmallProgram
    putStrLn "Testing optimizations:"
    putStrLn $ "testAddZeroLeft: " ++ show testAddZeroLeft
    putStrLn $ "testAddZeroRight: " ++ show testAddZeroRight
    putStrLn $ "testMultUnitLeft: " ++ show testMultUnitLeft
    putStrLn $ "testMultUnitRight: " ++ show testMultUnitRight
    putStrLn $ "testMultZeroLeft: " ++ show testMultZeroLeft
    putStrLn $ "testMultZeroRight: " ++ show testMultZeroRight
    putStrLn $ "testDivUnit: " ++ show testDivUnit
    putStrLn $ "testModUnit: " ++ show testModUnit
    putStrLn $ "testEval: " ++ show testEval
    putStrLn $ "testComplexEval: " ++ show testComplexEval
    putStrLn $ "testLogicalOr: " ++ show testLogicalOr
    putStrLn $ "testLogicalAnd: " ++ show testLogicalAnd
    putStrLn $ "testRemoveWhileLoop: " ++ show testRemoveWhileLoop
    putStrLn $ "testRemoveIfStatement: " ++ show testRemoveIfStatement
