{
module C.Lexer where
import C.Types
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
