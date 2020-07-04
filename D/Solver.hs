module D.Solver (main) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Control.Monad (foldM)
  import Data.List (sortOn, (\\), find)

  import B.Parser

  extractVars :: Exp -> [Exp]
  extractVars x = Set.toList $ go x
    where
      go :: Exp -> Set.Set Exp
      go x@(Var _) = Set.singleton x
      go (Impl a b) = Set.union (go a) (go b)
      go (Disj a b) = Set.union (go a) (go b)
      go (Conj a b) = Set.union (go a) (go b)
      go (Neg x) = go x

  eval :: Map.Map Exp Bool -> Exp -> Bool
  eval p x = go x
    where
      go :: Exp -> Bool
      go x@(Var _) = case Map.lookup x p of Just v -> v
      go (Impl a b) = not (go a) || go b
      go (Disj a b) = go a || go b
      go (Conj a b) = go a && go b
      go (Neg x) = not (go x)

  minimalSolution :: Exp -> Maybe (Bool,[Exp])
  minimalSolution x = find (uncurry check) (zip (repeat True) subsets ++ zip (repeat False) subsets)
    where
      variables :: [Exp]
      variables = extractVars x

      subsets :: [[Exp]]
      subsets = sortOn length $ foldM (\b a -> [b,a:b]) [] variables

      generatePs :: [Exp] -> Bool -> [Map.Map Exp Bool]
      generatePs h v = foldM kleisli initialP $ variables \\ h
        where
          kleisli :: Map.Map Exp Bool -> Exp -> [Map.Map Exp Bool]
          kleisli a var = [(Map.insert var False a),(Map.insert var True a)]

          initialP :: Map.Map Exp Bool
          initialP = Map.fromList $ zip h (repeat v)

      check :: Bool -> [Exp] -> Bool
      check r h = all (\p -> eval p x == r) (generatePs h r)

  generateProof :: [Exp]
  generateProof = []

  neg :: Exp -> Exp
  neg (Neg x) = x
  neg x = (Neg x)

  solve :: String -> [String]
  solve line = case minimalSolution statement of
    Nothing    -> lines ":("
    Just (r,h) ->
      let statement' = if r then Stmt statement h else Stmt (neg statement) (map neg h) in
        map show $ statement' : generateProof
    where
      statement :: Exp
      statement = parse line

  main :: IO ()
  main = do
    line <- getLine
    putStr $ (unlines . solve) line