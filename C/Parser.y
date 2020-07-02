{
module C.Parser where
import C.Lexer
import C.Types
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
}