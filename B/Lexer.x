{
module B.Lexer where
}

%wrapper "basic"

tokens :-

    $white+           ;
    \|\-              { \s -> TSym s }
    [!\,\&\|\(\)]     { \s -> TSym s }
    \->               { \s -> TSym s }
    [A-Z][0-9A-Z\']*  { \s -> TVar s }

{
data Token
    = TSym String
    | TVar String
    deriving (Eq,Show)
}
