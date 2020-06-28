module C.Solver where
import C.Parser

foo :: [String] -> [String]
foo = map (show . parse)

main :: IO ()
main = interact (unlines . foo . lines)