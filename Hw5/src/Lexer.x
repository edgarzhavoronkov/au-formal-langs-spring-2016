{
	module Lexer (Token(..), AlexPosn(..), scanTokens) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                             ;
    $digit+                             { tok (\p s -> TIntLiteral p (read s)) }
    "skip"                              { tok (\p s -> TSkip p) }
    "read"                              { tok (\p s -> TRead p) }
    "write"                             { tok (\p s -> TWrite p) }
    "while"                             { tok (\p s -> TWhile p) }
	"do"								{ tok (\p s -> TDo p) }
    "if"                                { tok (\p s -> TIf p) }
    "then"                              { tok (\p s -> TThen p) }
    "else"                              { tok (\p s -> TElse p) }
    ":="                                { tok (\p s -> TAssign p) }
    ";"                                 { tok (\p s -> TColon p) }
    "+"                                 { tok (\p s -> TPlus p) }
    "-"                                 { tok (\p s -> TMinus p) }
    "*"                                 { tok (\p s -> TMult p) }
    "/"                                 { tok (\p s -> TDiv p) }
    "%"                                 { tok (\p s -> TMod p) }
    "=="                                { tok (\p s -> TEq p) }
    "!="                                { tok (\p s -> TNeq p) }
    ">"                                 { tok (\p s -> TGt p) }
    ">="                                { tok (\p s -> TGeq p) }
    "<"                                 { tok (\p s -> TLt p) }
    "<="                                { tok (\p s -> TLeq p) }
    "&&"                                { tok (\p s -> TAnd p) }
    "||"                                { tok (\p s -> TOr p) }
    "("                                 { tok (\p s -> TLeftBracket p) }
    ")"                                 { tok (\p s -> TRightBracket p) }
    $alpha[$alpha $digit \_ \']*        { tok (\p s -> TVar p s ) }

{

tok f p s = f p s

data Token = TDo AlexPosn
			| TOr AlexPosn
			| TEq AlexPosn
			| TGt AlexPosn
			| TLt AlexPosn
			| TIf AlexPosn
			| TDiv AlexPosn
			| TMod AlexPosn
			| TNeq AlexPosn
			| TGeq AlexPosn
			| TLeq AlexPosn
			| TAnd AlexPosn
			| TSkip AlexPosn
			| TPlus AlexPosn
			| TMult AlexPosn
			| TRead AlexPosn
			| TThen AlexPosn
			| TElse AlexPosn
			| TWrite AlexPosn
			| TMinus AlexPosn
			| TWhile AlexPosn
			| TAssign AlexPosn
			| TVar AlexPosn String
			| TIntLiteral AlexPosn Int
			| TLeftBracket AlexPosn
			| TRightBracket AlexPosn
			| TColon AlexPosn deriving Eq

instance Show Token where
    show (TOr (AlexPn x y z)) = "Or(" ++ show z ++ ", " ++ show (z + length "||" - 1) ++ ")"
    show (TEq (AlexPn x y z)) = "Eq(" ++ show z ++ ", " ++ show (z + length "==" - 1) ++ ")"
    show (TGt (AlexPn x y z)) = "Gt(" ++ show z ++ ", " ++ show (z + length ">" - 1) ++ ")"
    show (TLt (AlexPn x y z)) = "Lt(" ++ show z ++ ", " ++ show (z + length "<" - 1) ++ ")"
    show (TIf (AlexPn x y z)) = "If(" ++ show z ++ ", " ++ show (z + length "If" - 1) ++ ")"
    show (TDiv (AlexPn x y z)) = "Div(" ++ show z ++ ", " ++ show (z + length "/" - 1) ++ ")"
    show (TMod (AlexPn x y z)) = "Mod(" ++ show z ++ ", " ++ show (z + length "%" - 1) ++ ")"
    show (TNeq (AlexPn x y z)) = "Neq(" ++ show z ++ ", " ++ show (z + length "!=" - 1) ++ ")"
    show (TGeq (AlexPn x y z)) = "Geq(" ++ show z ++ ", " ++ show (z + length ">=" - 1) ++ ")"
    show (TLeq (AlexPn x y z)) = "Leq(" ++ show z ++ ", " ++ show (z + length "<=" - 1) ++ ")"
    show (TAnd (AlexPn x y z)) = "And(" ++ show z ++ ", " ++ show (z + length "&&" - 1) ++ ")"
    show (TSkip (AlexPn x y z)) = "Skip(" ++ show z ++ ", " ++ show (z + length "skip" - 1) ++ ")"
    show (TPlus (AlexPn x y z)) = "Plus(" ++ show z ++ ", " ++ show (z + length "+" - 1) ++ ")"
    show (TMult (AlexPn x y z)) = "Mult(" ++ show z ++ ", " ++ show (z + length "*" - 1) ++ ")"
    show (TRead (AlexPn x y z)) = "Read(" ++ show z ++ ", " ++ show (z + length "read" - 1) ++ ")"
    show (TThen (AlexPn x y z)) = "Then(" ++ show z ++ ", " ++ show (z + length "then" - 1) ++ ")"
    show (TElse (AlexPn x y z)) = "Else(" ++ show z ++ ", " ++ show (z + length "else" - 1) ++ ")"
    show (TWrite (AlexPn x y z)) = "Write(" ++ show z ++ ", " ++ show (z + length "write" - 1) ++ ")"
    show (TMinus (AlexPn x y z)) = "Minus(" ++ show z ++ ", " ++ show (z + length "-" - 1) ++ ")"
    show (TWhile (AlexPn x y z)) = "While(" ++ show z ++ ", " ++ show (z + length "while" - 1) ++ ")"
    show (TAssign (AlexPn x y z)) = "Assign(" ++ show z ++ ", " ++ show (z + length ":=" - 1) ++ ")"
    show (TVar (AlexPn x y z) str) = "Var(" ++ show str ++ ", " ++ show z ++ ", " ++ show (z + length str - 1) ++ ")"
    show (TIntLiteral (AlexPn x y z) num) = "IntLit(" ++ show num ++ ", " ++ show z ++ ", " ++ show (z + (length $ show num) - 1) ++ ")"
    show (TLeftBracket (AlexPn x y z)) = "LeftBracket(" ++ show z ++ ", " ++ show (z + length "(" - 1) ++ ")"
    show (TRightBracket (AlexPn x y z)) = "RightBracket(" ++ show z ++ ", " ++ show (z + length ")" - 1) ++ ")"
    show (TColon (AlexPn x y z)) = "Colon(" ++ show z ++ ", " ++ show (z + length ";" - 1) ++ ")"

scanTokens = alexScanTokens
}
