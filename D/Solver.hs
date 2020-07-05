module D.Solver (main) where
  import qualified Data.List as List
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Control.Monad (foldM)

  import D.Parser
  import D.Types

  extractVars :: Exp -> [Exp]
  extractVars x = Set.toList $ go x
    where
      go :: Exp -> Set.Set Exp
      go x@(EVar _) = Set.singleton x
      go (EImpl a b) = Set.union (go a) (go b)
      go (EDisj a b) = Set.union (go a) (go b)
      go (EConj a b) = Set.union (go a) (go b)
      go (ENeg x) = go x

  eval :: Map.Map Exp Bool -> Exp -> Bool
  eval p x = go x
    where
      go :: Exp -> Bool
      go x@(EVar _) = case Map.lookup x p of Just v -> v
      go (EImpl a b) = not (go a) || go b
      go (EDisj a b) = go a || go b
      go (EConj a b) = go a && go b
      go (ENeg x) = not (go x)

  minimalSolution :: Exp -> Maybe (Bool,[Exp])
  minimalSolution x = List.find (uncurry check) (zip (repeat True) subsets ++ zip (repeat False) subsets)
    where
      variables :: [Exp]
      variables = extractVars x

      subsets :: [[Exp]]
      subsets = List.sortOn length $ foldM (\b a -> [b,a:b]) [] variables

      generatePs :: [Exp] -> Bool -> [Map.Map Exp Bool]
      generatePs h v = foldM kleisli initialP $ (List.\\) variables h
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
  neg (ENeg x) = x
  neg x = (ENeg x)

  solve :: String -> [String]
  solve line = case minimalSolution statement of
    Nothing    -> lines ":("
    Just (r,h) ->
      let statement' = if r then Statement statement h else Statement (neg statement) (map neg h) in
        show statement' : map show generateProof
    where
      statement :: Exp
      statement = parse line

  main :: IO ()
  main = do
    line <- getLine
    putStr $ (unlines . solve) line