{
module C.Parser where
import C.Lexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%right '->'
%left '|'
%left '&'
%right '!'
%left '='
%left '+'
%left '*'
%left 'i'

%token 
    lower           { TLower $$ }
    upper           { TUpper $$ }
    zero            { TZero }
    '|-'            { TSym "|-" }
    '->'            { TSym "->" }
    '|'             { TSym "|" }
    '&'             { TSym "&" }
    '!'             { TSym "!" }
    '('             { TSym "(" }
    ')'             { TSym ")" }
    '@'             { TSym "@" }
    '?'             { TSym "?" }
    '.'             { TSym "." }
    '='             { TSym "=" }
    '+'             { TSym "+" }
    '*'             { TSym "*" }
    'i'             { TSym "'" }

%%

Line        : Header                        { $1 }
            | Expression                    { $1 }

Header      : '|-' Expression               { EHead $2 }

Expression  : Disj                          { $1 }
            | Disj '->' Expression          { EImpl $1 $3 }

Disj        : Conj                          { $1 }
            | Disj '|' Conj                 { EDisj $1 $3 }

Conj        : Unary                         { $1 }
            | Conj '&' Unary                { EConj $1 $3 }

Unary       : Predicate                     { $1 }
            | '!' Unary                     { ENeg $2 }
            | '(' Expression ')'            { $2 }
            | '@' Variable '.' Expression   { EForall $2 $4 }
            | '?' Variable '.' Expression   { EExists $2 $4 }

Variable    : lower                         { EVar $1 }

Predicate   : upper                         { EPred $1 }
            | Term '=' Term                 { EEquals $1 $3 }

Term        : Summand                       { $1 }
            | Term '+' Summand              { ESum $1 $3 }

Summand     : Mult                          { $1 }
            | Summand '*' Mult              { EMul $1 $3 }

Mult        : Variable                      { $1 }
            | '(' Term ')'                  { $2 }
            | zero                          { EZero }
            | Mult 'i'                      { EInc $1 }

{
parse :: String -> Exp
parse = parseTokens . alexScanTokens

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
    = EImpl Exp Exp
    | EDisj Exp Exp
    | EConj Exp Exp
    | ENeg Exp
    | EForall Exp Exp
    | EExists Exp Exp
    | EVar Char
    | EPred Char
    | EEquals Exp Exp
    | ESum Exp Exp
    | EMul Exp Exp
    | EZero
    | EInc Exp
    | EHead Exp
    deriving (Eq,Ord)

showBinOp op l r = "(" ++ (show l) ++ op ++ (show r) ++ ")"

instance Show Exp where
    show (EHead x) = "|-" ++ (show x)
    show (EImpl l r) = showBinOp "->" l r
    show (EDisj l r) = showBinOp "|" l r
    show (EConj l r) = showBinOp "&" l r
    show (EEquals l r) = showBinOp "=" l r
    show (ESum l r) = showBinOp "+" l r
    show (EMul l r) = showBinOp "*" l r
    show (ENeg x) = "(!" ++ (show x) ++ ")"
    show (EForall v p) = "(@" ++ (show v) ++ "." ++ (show p) ++ ")"
    show (EExists v p) = "(?" ++ (show v) ++ "." ++ (show p) ++ ")"
    show (EVar c) = [c]
    show (EPred c) = [c]
    show (EZero) = "0"
    show (EInc x) = (show x) ++ "'"
}