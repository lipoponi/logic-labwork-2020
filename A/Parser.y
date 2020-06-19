{
module A.Parser (parseString, parseError) where
import A.Lexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%right '->'
%left '|'
%left '&'
%left '!'

%token 
      var             { TVar $$ }
      '->'            { TSym "->" }
      '|'             { TSym "|" }
      '&'             { TSym "&" }
      '!'             { TSym "!" }
      '('             { TSym "(" }
      ')'             { TSym ")" }

%%

Exp : Exp '->' Exp { Impl $1 $3 }
    | Exp '|' Exp { Disj $1 $3 }
    | Exp '&' Exp { Conj $1 $3 }
    | '!' Exp { Neg $2 }
    | '(' Exp ')' { $2 }
    | var { Var $1 }

{
parseString :: String -> Exp
parseString = parseTokens . alexScanTokens

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
    = Impl Exp Exp
    | Disj Exp Exp
    | Conj Exp Exp
    | Neg Exp
    | Var String

showBinOp op l r = "(" ++ op ++ "," ++ (show l) ++ "," ++ (show r) ++ ")"

instance Show Exp where
    show (Impl l r) = showBinOp "->" l r
    show (Disj l r) = showBinOp "|" l r
    show (Conj l r) = showBinOp "&" l r
    show (Neg inner) = "(!" ++ (show inner) ++ ")"
    show (Var name) = name
}