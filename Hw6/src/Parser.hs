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
