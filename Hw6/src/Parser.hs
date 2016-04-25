module Parser where

import Expr

import Control.Monad
import Text.Parsec.Language
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
    emptyDef    { Token.commentStart    = ""
                , Token.commentEnd      = ""
                , Token.commentLine     = ""
                , Token.nestedComments  = False
                , Token.caseSensitive   = True
                , Token.identStart      = letter
                , Token.identLetter     = alphaNum
                , Token.reservedNames   = [ "skip"
                                          , ";"
                                          , "write"
                                          , "read"
                                          , "while"
                                          , "do"
                                          , "if"
                                          , "then"
                                          , "else"
                                          ]
                , Token.reservedOpNames = [ "+"
                                          , "-"
                                          , "*"
                                          , "/"
                                          , ":="
                                          , "%"
                                          , "=="
                                          , "!="
                                          , ">"
                                          , ">="
                                          , "<"
                                          , "<="
                                          , "&&"
                                          , "||"
                                          ]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
semi       = Token.semi       lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    spaces
    cond <- expression
    reserved "then"
    spaces
    action1 <- statement
    reserved "else"
    spaces
    action2 <- statement
    return $ IfCond cond action1 action2

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- expression
    spaces
    reserved "do"
    action <- statement
    return $ WhileLoop cond action

--fucked up assignment!
assignStmt :: Parser Stmt
assignStmt = do
    var <- expression
    spaces
    reservedOp ":="
    spaces
    expr <- expression
    return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = do
    reserved "skip"
    spaces
    return Skip

readStmt :: Parser Stmt
readStmt = do
    reserved "read"
    spaces
    e <- expression
    return $ Read e

writeStmt :: Parser Stmt
writeStmt = do
    reserved "write"
    spaces
    e <- expression
    return $ Write e

statement :: Parser Stmt
statement = do
    ss <- sepBy1 statement' (reserved ";")
    if length ss == 1
        then return $ head ss
        else return $ foldr1 Colon ss

statement' :: Parser Stmt
statement' = assignStmt
            <|> writeStmt
            <|> readStmt
            <|> whileStmt
            <|> ifStmt
            <|> skipStmt

expression :: Parser Expr
expression = buildExpressionParser operators term

term = fmap Var identifier
        <|> fmap Num integer
        <|> parens expression

operators = [ [Infix (reservedOp "*"  >> return (BinOp Mul)) AssocLeft,
              Infix (reservedOp "/"  >> return (BinOp Div)) AssocLeft,
              Infix (reservedOp "%"  >> return (BinOp Mod)) AssocLeft]

            , [Infix (reservedOp "+"  >> return (BinOp Add)) AssocLeft,
              Infix (reservedOp "-"  >> return (BinOp Sub)) AssocLeft]

            , [Infix (reservedOp "==" >> return (BinOp Eq)) AssocNone,
              Infix (reservedOp "!=" >> return (BinOp Neq)) AssocNone,
              Infix (reservedOp ">"  >> return (BinOp Gt)) AssocNone,
              Infix (reservedOp ">=" >> return (BinOp Geq)) AssocNone,
              Infix (reservedOp "<"  >> return (BinOp Lt)) AssocNone,
              Infix (reservedOp "<=" >> return (BinOp Leq)) AssocNone,
              Infix (reservedOp "&&" >> return (BinOp And)) AssocNone,
              Infix (reservedOp "||" >> return (BinOp Or)) AssocNone]
            ]

parser :: Parser Stmt
parser = whiteSpace >> statement

parseString :: String -> Stmt
parseString str =
    case parse parser "" str of
        Left e -> error $ show e
        Right r -> r

--Pretty printing
pprint' :: Expr -> String
pprint' (Var i) = i
pprint' (Num n) = show n
pprint' (BinOp Add e1 e2) = pprint' e1 ++ " + " ++ pprint' e2
pprint' (BinOp Sub e1 e2) = pprint' e1 ++ " - " ++ pprint' e2
pprint' (BinOp Mul e1 e2) = pprint' e1 ++ " * " ++ pprint' e2
pprint' (BinOp Div e1 e2) = pprint' e1 ++ " / " ++ pprint' e2
pprint' (BinOp Mod e1 e2) = pprint' e1 ++ " % " ++ pprint' e2
pprint' (BinOp Eq e1 e2) = pprint' e1 ++ " == " ++ pprint' e2
pprint' (BinOp Neq e1 e2) = pprint' e1 ++ " != " ++ pprint' e2
pprint' (BinOp Gt e1 e2) = pprint' e1 ++ " > " ++ pprint' e2
pprint' (BinOp Geq e1 e2) = pprint' e1 ++ " >= " ++ pprint' e2
pprint' (BinOp Lt e1 e2) = pprint' e1 ++ " < " ++ pprint' e2
pprint' (BinOp Leq e1 e2) = pprint' e1 ++ " <= " ++ pprint' e2
pprint' (BinOp And e1 e2) = pprint' e1 ++ " && " ++ pprint' e2
pprint' (BinOp Or e1 e2) = pprint' e1 ++ " || " ++ pprint' e2


pprint :: Stmt -> IO ()
pprint = helper ""
    where
        helper :: String -> Stmt -> IO ()
        helper indent Skip =
            putStr $ indent ++ "skip"
        helper indent (Assign (Var v) e) =
            putStr $ indent ++ v ++ " := " ++ pprint' e
        helper indent (Colon s1 s2) = do
            helper indent s1
            putStrLn ";"
            helper indent s2
        helper indent (Write e) =
            putStr $ indent ++ "write " ++ pprint' e
        helper indent (Read e) =
            putStr $ indent ++ "read " ++ pprint' e
        helper indent (WhileLoop e s) = do
            putStrLn $ indent ++ "while " ++ pprint' e ++ " do"
            helper ('\t':indent) s
        helper indent (IfCond e s1 s2) = do
            putStrLn $ indent ++ "if " ++ pprint' e
            putStrLn $ indent ++ "then"
            helper ('\t' : indent) s1
            putStrLn $ ('\n':indent) ++ "else"
            helper ('\t' : indent) s2

--TODO - normal evaluations
simplify :: Expr -> Expr
simplify (Var x) = Var x
simplify (Num i) = Num i
--neutral elements under addition
simplify (BinOp Add e (Num 0)) = simplify e
simplify (BinOp Add (Num 0) e) = simplify e
--subtraction
simplify (BinOp Sub e (Num 0)) = simplify e
--multiplication
simplify (BinOp Mul e (Num 1)) = simplify e
simplify (BinOp Mul (Num 1) e) = simplify e
--mult by zero
simplify (BinOp Mul (Num 0) e) = Num 0
simplify (BinOp Mul e (Num 0)) = Num 0
--division?
simplify (BinOp Div (Num 0) e) = Num 0
simplify (BinOp Div e (Num 1)) = simplify e
simplify (BinOp Div e1 e2) | e1 == e2 = Num 1
--modulo
simplify (BinOp Mod (Num 0) e) = Num 0
simplify (BinOp Mod e (Num 1)) = Num 0
--numeric
simplify (BinOp Add (Num x) (Num y)) = Num (x + y)
simplify (BinOp Sub (Num x) (Num y)) = Num (x - y)
simplify (BinOp Mul (Num x) (Num y)) = Num (x * y)
simplify (BinOp Div (Num x) (Num y)) = Num (x `div` y)
simplify (BinOp Mod (Num x) (Num y)) = Num (x `mod` y)
--associativity for addition with little bit of commutativity
simplify (BinOp Add (Num x) (BinOp Add (Num y) ex)) = BinOp Add (Num (x + y)) (simplify ex)
simplify (BinOp Add (Num x) (BinOp Add ex (Num y))) = BinOp Add (Num (x + y)) (simplify ex)
simplify (BinOp Add (BinOp Add (Num x) ex) (Num y)) = BinOp Add (Num (x + y)) (simplify ex)
simplify (BinOp Add (BinOp Add ex (Num x)) (Num y)) = BinOp Add (Num (x + y)) (simplify ex)
--associativity for multiplication with little bit of commutativity
simplify (BinOp Mul (Num x) (BinOp Mul (Num y) ex)) = BinOp Mul (Num (x * y)) (simplify ex)
simplify (BinOp Mul (Num x) (BinOp Mul ex (Num y))) = BinOp Mul (Num (x * y)) (simplify ex)
simplify (BinOp Mul (BinOp Mul (Num x) ex) (Num y)) = BinOp Mul (Num (x * y)) (simplify ex)
simplify (BinOp Mul (BinOp Mul ex (Num x)) (Num y)) = BinOp Mul (Num (x * y)) (simplify ex)
--distibutivity
simplify (BinOp Mul (Num x) (BinOp Add e1 e2)) = BinOp Add (BinOp Mul (Num x) (simplify e1)) (BinOp Mul (Num x) (simplify e2))
--recursion launcher
simplify (BinOp op e1 e2) = BinOp op (simplify e1) (simplify e2)
--
totalSimplify :: Expr -> Expr
totalSimplify expr = helper expr (Num 0)
    where
        helper cur prev | cur == prev = cur
                        | otherwise = let cur' = simplify cur in helper cur' cur

optimize :: Stmt -> Stmt
optimize Skip = Skip
optimize (Colon s1 s2) = Colon (optimize s1) (optimize s2)
optimize (Assign e1 e2) = Assign e1 (totalSimplify e2)
optimize (Read e) = Read e
optimize (Write e) = Write (totalSimplify e)
optimize (WhileLoop e s) = WhileLoop (totalSimplify e) (optimize s)
optimize (IfCond e s1 s2) = IfCond (totalSimplify e) (optimize s1) (optimize s2)
