module A (solve) where
import Lexer
import Parser

solve = do
    contents <- getContents;
    (putStrLn . show . parseTokens . alexScanTokens) contents;