module A (solve) where
import Parser

solve = do
    contents <- getContents;
    (putStrLn . show . parseString) contents;