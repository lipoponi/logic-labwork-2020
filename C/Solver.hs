module C.Solver (main) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Data.List (sort)

  import C.Common
  import C.Schemes
  import C.Axioms
  import C.Rules

  annotate :: Int -> Exp -> Map.Map Exp Int -> Map.Map Exp (Int,Int) -> Either String String
  annotate index x sa sc = case result of
    Left s   -> Left $ "[" ++ (show index) ++ ". " ++ s ++ "] " ++ (show x)
    Right [] -> Right $ "Expression " ++ (show index) ++ " is not proved."
    Right es -> case sort es of
      (_,e):_ -> Right $ "Expression " ++ (show index) ++ ": " ++ e
    where
      result :: Either Annotation Errors
      result = return [] >>= checkSchemes x >>= checkAxioms x >>= checkRules x sa sc

  go :: Exp -> [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> [String]
  go header [x] index sa sb sc = (++[]) $ case annotate index x sa sc of
    Left s  -> if header == x then [s] else s:["The proof proves different expression."]
    Right s -> [s]
  go header (x:xs) index sa sb sc = case annotate index x sa sc of
    Left s  -> s : go header xs (succ index) newSA newSB newSC
    Right s -> s : []
    where
      newSA = nextSA x index sa
      newSB = nextSB x index sb
      newSC = nextSC x index sa sb sc

  solve :: [String] -> [String]
  solve contents = case header of EHead statement -> show header : go statement proof 1 Map.empty Map.empty Map.empty
    where
      header :: Exp
      header = parse $ head contents

      proof :: [Exp]
      proof = map parse $ tail contents

  main :: IO ()
  main = interact (unlines . solve . lines)
