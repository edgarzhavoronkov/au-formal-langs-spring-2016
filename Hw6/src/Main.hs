import Parser
import Expr

showAST :: (String -> Stmt) -> String -> IO ()
showAST parser input = do
    let ast = parser input
    print ast

main :: IO ()
main = do
    input <- getContents
    showAST parseString input
