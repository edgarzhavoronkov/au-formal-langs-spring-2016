import Parser
import Expr

import System.Environment

showAST :: (String -> Stmt) -> String -> IO ()
showAST parser input = do
    let ast = parser input
    print ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            showAST parseString input
        ["-o"] -> do
            input <- getContents
            showAST (optimize . parseString) input
            pprint $ optimize $ parseString input
        _ -> putStrLn "Wrong key! Use -o for optimized mode"
