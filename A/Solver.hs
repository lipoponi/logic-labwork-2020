module A.Solver (main) where
import A.Parser

main = do
    contents <- getContents;
    (putStrLn . show . parseString) contents;