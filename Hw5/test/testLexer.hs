module Main where

import Lexer

testSimpleLexeme :: Bool
testSimpleLexeme = head (scanTokens "x") == TVar (AlexPn 0 1 1) "x"

testSmallProgram :: Bool
testSmallProgram = scanTokens prog == tkns
    where
        prog = "read x; if y + 1 == x then write y else skip"
        tkns = [TRead (AlexPn 0 1 1),TVar (AlexPn 5 1 6) "x",
                TColon (AlexPn 6 1 7),TIf (AlexPn 8 1 9),
                TVar (AlexPn 11 1 12) "y",TPlus (AlexPn 13 1 14),
                TIntLiteral (AlexPn 15 1 16) 1,TEq (AlexPn 17 1 18),
                TVar (AlexPn 20 1 21) "x",TThen (AlexPn 22 1 23),
                TWrite (AlexPn 27 1 28),TVar (AlexPn 33 1 34) "y",
                TElse (AlexPn 35 1 36),TSkip (AlexPn 40 1 41)]


main :: IO ()
main = do
    putStrLn $ "testSimpleLexeme: " ++ show testSimpleLexeme
    putStrLn $ "testSmallProgram: " ++ show testSmallProgram
