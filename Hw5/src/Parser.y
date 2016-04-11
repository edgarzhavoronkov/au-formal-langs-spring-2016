{
module Parser where

import Lexer
import Expr

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    skip        { TSkip _           }
    read        { TRead _           }
    write       { TWrite _          }
    while       { TWhile _          }
    do          { TDo _             }
    if          { TIf _             }
    then        { TThen _           }
    else        { TElse _           }
    NUM         { TIntLiteral _ $$  }
    VAR         { TVar _ $$         }
    ':='        { TAssign _         }
    ';'         { TColon _          }
    '+'         { TPlus _           }
    '-'         { TMinus _          }
    '*'         { TMult _           }
    '/'         { TDiv _            }
    '%'         { TMod _            }
    '=='        { TEq _             }
    '!='        { TNeq _            }
    '>'         { TGt _             }
    '>='        { TGeq _            }
    '<'         { TLt _             }
    '<='        { TLeq _            }
    '&&'        { TAnd _            }
    '||'        { TOr _             }
    '('         { TLeftBracket _    }
    ')'         { TRightBracket _   }

%nonassoc '&&' '||'
%nonassoc '==' '!='
%nonassoc '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/' '%'

%%

Stmt : skip                                 { Skip }
     | VAR ':=' Expr                        { Assign (Var $1) $3 }
     | Stmt ';' Stmt                        { Colon $1 $3 }
     | write Expr                           { Write $2 }
     | read Expr                            { Read $2 }
     | while Expr do Stmt                   { WhileLoop $2 $4 }
     | if Expr then Stmt else Stmt          { IfCond $2 $4 $6 }

Expr : VAR                                  { Var $1 }
     | NUM                                  { Num $1 }
     | '(' Expr ')'                         { $2 }
     | Expr '*' Expr                        { BinOp Mul $1 $3 }
     | Expr '/' Expr                        { BinOp Div $1 $3 }
     | Expr '%' Expr                        { BinOp Mod $1 $3 }
     | Expr '+' Expr                        { BinOp Add $1 $3 }
     | Expr '-' Expr                        { BinOp Sub $1 $3 }
     | Expr '>' Expr                        { BinOp Gt  $1 $3 }
     | Expr '>=' Expr                       { BinOp Geq $1 $3 }
     | Expr '<' Expr                        { BinOp Lt  $1 $3 }
     | Expr '<=' Expr                       { BinOp Leq $1 $3 }
     | Expr '==' Expr                       { BinOp Eq  $1 $3 }
     | Expr '!=' Expr                       { BinOp Neq $1 $3 }
     | Expr '&&' Expr                       { BinOp And $1 $3 }
     | Expr '||' Expr                       { BinOp Or  $1 $3 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Stmt
parseExpr = parse . scanTokens
}
