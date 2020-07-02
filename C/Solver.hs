module C.Solver (main) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Data.List (sort)

  import C.Common
  import C.Schemes
  import C.Axioms
  import C.Rules

  annotate :: Int -> Exp -> Either String String
  annotate index x = case result of
    Left s   -> Left $ "[" ++ (show index) ++ ". " ++ s ++ "] " ++ (show x)
    Right [] -> Right $ "Expression " ++ (show index) ++ " is not proved."
    Right es -> case sort es of
      (_,e):_ -> Right $ "Expression " ++ (show index) ++ ": " ++ e
    where
      result :: Either Annotation Errors
      result = return [] >>= checkSchemes x >>= checkAxioms x >>= checkRules x

  go :: Exp -> [Exp] -> Int -> [String]
  go header [x] index = (:[]) $ case annotate index x of
    Left s  -> if header == x then s else "The proof proves different expression."
    Right s -> s
  go header (x:xs) index = case annotate index x of
    Left s  -> s : go header xs (succ index)
    Right s -> s : []

  solve :: [String] -> [String]
  solve contents = case header of EHead statement -> show header : go statement proof 1
    where
      header :: Exp
      header = parse $ head contents

      proof :: [Exp]
      proof = map parse $ tail contents

  main :: IO ()
  main = interact (unlines . solve . lines)
