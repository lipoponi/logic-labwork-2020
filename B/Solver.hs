module B.Solver (main) where
import B.Parser
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import Data.Char (isSpace, isUpper)

axioms :: [Exp]
axioms = map parse [
  "A->B->A",
  "(A->B)->(A->B->C)->A->C",
  "A->B->A&B",
  "A&B->A",
  "A&B->B",
  "A->A|B",
  "B->A|B",
  "(A->C)->(B->C)->A|B->C",
  "(A->B)->(A->!B)->!A",
  "!!A->A"]

getAxiom :: Exp -> Maybe Int
getAxiom x = helper 1 axioms
  where
    helper :: Int -> [Exp] -> Maybe Int
    helper i [] = Nothing
    helper i (a:as) = if fits a x then Just i else helper (i + 1) as

    fits :: Exp -> Exp -> Bool
    fits a x = case aliases a x of
      Nothing -> False
      _ -> True

    aliases :: Exp -> Exp -> Maybe (Map.Map String Exp)
    aliases (Var name) x = Just (Map.singleton name x)
    aliases (Neg ain) (Neg xin) = aliases ain xin
    aliases (Impl al ar) (Impl xl xr) = stepIn al ar xl xr
    aliases (Disj al ar) (Disj xl xr) = stepIn al ar xl xr
    aliases (Conj al ar) (Conj xl xr) = stepIn al ar xl xr
    aliases _ _ = Nothing

    stepIn :: Exp -> Exp -> Exp -> Exp -> Maybe (Map.Map String Exp)
    stepIn al ar xl xr = case (aliases al xl,aliases ar xr) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just l, Just r) -> merge l r

    merge :: Map.Map String Exp -> Map.Map String Exp -> Maybe (Map.Map String Exp)
    merge l r = let
      fn k rv = case (Map.lookup k l) of
        (Just lv) -> if lv == rv then Just lv else Nothing
        Nothing -> Just rv
      in case (Map.traverseWithKey fn r) of
        Nothing -> Nothing
        _ -> Just (Map.union l r)

insertIfAbsent :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
insertIfAbsent = Map.insertWith (flip const)

newA :: Exp -> Int -> Map.Map Exp Int -> Map.Map Exp Int
newA = insertIfAbsent

newB :: Exp -> Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Map.Map Exp Int)
newB x index b = Map.delete x tmpB
  where
    tmpB = case x of
      Impl l r -> Map.insertWith (flip Map.union) l (Map.singleton r index) b -- check optimization
      _ -> b

newC :: Exp -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> Map.Map Exp (Int,Int)
newC x index a b c = Map.foldrWithKey (\k rIndex -> insertIfAbsent k (index,rIndex)) tmpC (Map.findWithDefault Map.empty x b)
  where
    tmpC = case x of
      Impl l r -> case Map.lookup l a of
        Just lIndex -> insertIfAbsent r (lIndex,index) c
        _ -> c
      _ -> c

isCorrect :: Exp -> [Exp] -> [Exp] -> Bool
isCorrect statement context proof = proof /= [] && last proof == statement && helper proof 1 Map.empty Map.empty Map.empty
  where
    hypoSet = Set.fromList context

    helper [] _ _ _ _ = True
    helper (x:xs) index a b c = good && (helper xs (succ index) (newA x index a) (newB x index b) (newC x index a b c))
      where
        good = Set.member x hypoSet || getAxiom x /= Nothing || Map.member x c

minimize :: [Exp] -> [Exp] -> [Exp]
minimize !context !proof = extract proof 1 $ helper proof 1 Map.empty Map.empty Map.empty
  where
    hypoSet :: Set.Set Exp
    hypoSet = Set.fromList context

    helper :: [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> Set.Set Int
    helper [x] index _ _ c =
      if Set.member x hypoSet || getAxiom x /= Nothing then
        Set.singleton index
      else
        Set.fromList $ a:b:index:[]
      where
        Just (a,b) = Map.lookup x c
    helper (x:xs) index a b c = suffix `seq`
      if Set.notMember index suffix || Set.member x hypoSet || getAxiom x /= Nothing then
        suffix
      else
        case Map.lookup x c of {Just (a,b) -> Set.insert a $ Set.insert b suffix}
      where
        suffix = (helper xs (succ index) (newA x index a) (newB x index b) (newC x index a b c))

    extract :: [Exp] -> Int -> Set.Set Int -> [Exp]
    extract [] _ _ = []
    extract (x:xs) index set = (if Set.member index set then (x:) else id) (extract xs (succ index) set)

annotate :: Exp -> [Exp] -> [Exp] -> [String]
annotate statement !context !proof = helper proof 1 Map.empty Map.empty Map.empty
  where
    hypoMap :: Map.Map Exp Int
    hypoMap = Map.fromList (zip context [1..])

    helper :: [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> [String]
    helper [] _ _ _ _ = []
    helper (x:xs) index a b c = (fn x index c) : (helper xs (succ index) (newA x index a) (newB x index b) (newC x index a b c))

    fn :: Exp -> Int -> Map.Map Exp (Int,Int) -> String
    fn x index c = "[" ++ (show index) ++ ". " ++ (
      case Map.lookup x hypoMap of
        (Just n) -> "Hypothesis " ++ (show n)
        Nothing -> case getAxiom x of
          (Just n) -> "Ax. sch. " ++ (show n)
          Nothing -> case Map.lookup x c of
            (Just (a,b)) -> "M.P. " ++ (show b) ++ ", " ++ (show a)
      ) ++ "] " ++ (show x)

foo :: [String] -> [String]
foo !contents =
  if isCorrect statement context proof then
    (show $ Stmt statement context) : (annotate statement context $ minimize context proof)
  else
    ["Proof is incorrect"]
  where
    (contextS:statementS:[]) = Split.splitOn "|-" (head contents)
    statement = parse statementS
    context = map parse (filter (any (not . isSpace)) (Split.splitOn "," contextS))
    proof = map parse $ tail contents

main :: IO ()
main = interact (unlines . foo . lines)