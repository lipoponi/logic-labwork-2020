{
module B.Parser where
import Data.Char (isSpace,isUpper,isDigit)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%right '->'
%left '|'
%left '&'
%right '!'

%token 
      var             { TVar $$ }
      '->'            { TSym "->" }
      '|'             { TSym "|" }
      '&'             { TSym "&" }
      '!'             { TSym "!" }
      '('             { TSym "(" }
      ')'             { TSym ")" }

%%

Expression : Expression '&' Expression  { Conj $1 $3 }
           | Expression '|' Expression  { Disj $1 $3 }
           | Expression '->' Expression { Impl $1 $3 }
           | '!' Expression             { Neg $2 }
           | '(' Expression ')'         { $2 }
           | var                        { Var $1 }

{
data Token
    = TSym String
    | TVar String
    deriving (Eq,Show)

scan :: String -> [Token]
scan [] = []
scan (c:cs) = go
  where
    var = case span (\x -> isUpper x || isDigit x || x == '\'') (c:cs) of
      (name,rest) -> (TVar name) : (scan rest)
    go | isSpace c = scan cs
       | c == '-'  = (TSym "->") : (scan (tail cs))
       | isUpper c = var
       | otherwise = (TSym [c]) : (scan cs)

parse :: String -> Exp
parse = parseTokens . scan

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
    = Stmt Exp [Exp]
    | Impl Exp Exp
    | Disj Exp Exp
    | Conj Exp Exp
    | Neg Exp
    | Var String
    deriving (Eq,Ord)

showBinOp op l r = "(" ++ (show l) ++ " " ++ op ++ " " ++ (show r) ++ ")"

instance Show Exp where
    show (Stmt exp []) = "|- " ++ (show exp)
    show (Stmt exp [h]) = (show h) ++ " " ++ (show (Stmt exp []))
    show (Stmt exp (h:hs)) = (show h) ++ ", " ++ (show (Stmt exp hs))
    show (Impl l r) = showBinOp "->" l r
    show (Disj l r) = showBinOp "|" l r
    show (Conj l r) = showBinOp "&" l r
    show (Neg inner) = "!" ++ (show inner)
    show (Var name) = name
}