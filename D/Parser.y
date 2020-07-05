{
module D.Parser (parse) where
import D.Types
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

Expression : Expression '&' Expression  { EConj $1 $3 }
           | Expression '|' Expression  { EDisj $1 $3 }
           | Expression '->' Expression { EImpl $1 $3 }
           | '!' Expression             { ENeg $2 }
           | '(' Expression ')'         { $2 }
           | var                        { EVar $1 }

{
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
}