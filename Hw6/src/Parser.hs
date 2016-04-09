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
    condExpr <- expression
    reserved "then"
    action1 <- statement
    reserved "else"
    action2 <- statement
    return $ IfCond condExpr action1 action2

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- expression
    reserved "do"
    action <- statement
    return $ WhileLoop cond action

--fucked up assignment!
assignStmt :: Parser Stmt
assignStmt = do
    var <- expression
    reservedOp ":="
    expr <- expression
    return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = do
    reserved "skip"
    return Skip

colonStmt :: Parser Stmt
colonStmt = do
    s1 <- statement
    reserved ";"
    s2 <- statement
    return $ Colon s1 s2

readStmt :: Parser Stmt
readStmt = do
    reserved "read"
    e <- expression
    return $ Read e

writeStmt :: Parser Stmt
writeStmt = do
    reserved "write"
    e <- expression
    return $ Write e

statement :: Parser Stmt
statement = skipStmt
            <|> assignStmt
            <|> colonStmt
            <|> writeStmt
            <|> readStmt
            <|> whileStmt
            <|> ifStmt

expression :: Parser Expr
expression = buildExpressionParser operators term

term = parens opExpression
        <|> fmap Var identifier
        <|> fmap Num integer

opExpression = do
    e1 <- expression
    op <- operator
    e2 <- expression
    return $ BinOp op e1 e2

operator = (reservedOp "==" >> return Eq)
        <|> (reservedOp "!=" >> return Neq)
        <|> (reservedOp ">" >> return Gt)
        <|> (reservedOp ">=" >> return Geq)
        <|> (reservedOp "<" >> return Lt)
        <|> (reservedOp "<=" >> return Leq)
        <|> (reservedOp "&&" >> return And)
        <|> (reservedOp "||" >> return Or)
        <|> (reservedOp "*" >> return Mul)
        <|> (reservedOp "/" >> return Div)
        <|> (reservedOp "%" >> return Mod)
        <|> (reservedOp "+" >> return Add)
        <|> (reservedOp "-" >> return Sub)

operators = [ [Infix (reservedOp "==" >> return (BinOp Eq)) AssocNone,
              Infix (reservedOp "!=" >> return (BinOp Neq)) AssocNone,
              Infix (reservedOp ">"  >> return (BinOp Gt)) AssocNone,
              Infix (reservedOp ">=" >> return (BinOp Geq)) AssocNone,
              Infix (reservedOp "<"  >> return (BinOp Lt)) AssocNone,
              Infix (reservedOp "<=" >> return (BinOp Leq)) AssocNone,
              Infix (reservedOp "&&" >> return (BinOp And)) AssocNone,
              Infix (reservedOp "||" >> return (BinOp Or)) AssocNone]

            , [Infix (reservedOp "*"  >> return (BinOp Mul)) AssocLeft,
              Infix (reservedOp "/"  >> return (BinOp Div)) AssocLeft,
              Infix (reservedOp "%"  >> return (BinOp Mod)) AssocLeft]

            , [Infix (reservedOp "+"  >> return (BinOp Add)) AssocLeft,
               Infix (reservedOp "-"  >> return (BinOp Sub)) AssocLeft]
            ]

parser :: Parser Stmt
parser = whiteSpace >> statement

parseString :: String -> Stmt
parseString str =
    case parse parser "" str of
        Left e -> error $ show e
        Right r -> r
