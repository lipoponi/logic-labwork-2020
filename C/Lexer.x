{
module C.Lexer where
}

%wrapper "basic"

tokens :-

    $white+                     ;
    [a-z]                       { \[c] -> TLower c }
    [A-Z]                       { \[c] -> TUpper c }
    0                           { \_ -> TZero }
    \|\-                        { TSym }
    \-\>                        { TSym }
    [\!\&\|\(\)\@\?\.\=\+\*\']  { TSym }

{
data Token
    = TLower Char
    | TUpper Char
    | TZero
    | TSym String
    deriving (Eq,Show)
}
