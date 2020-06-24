module B.Solver (main) where
import B.Parser
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import Data.Char (isSpace)

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

nextState :: Exp -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> (Map.Map Exp Int,Map.Map Exp (Map.Map Exp Int),Map.Map Exp (Int,Int))
nextState x index a b c = (newA,newB,newC)
  where
    insertIfAbsent = Map.insertWith (flip const)
    newA = insertIfAbsent x index a
    tmpB = case x of
      Impl l r -> Map.insertWith (flip Map.union) l (Map.singleton r index) b
      _ -> b
    newB = Map.delete x tmpB
    tmpC = case x of
      Impl l r -> case Map.lookup l a of
        Just lIndex -> insertIfAbsent r (lIndex,index) c
        _ -> c
      _ -> c
    newC = Map.foldrWithKey (\k rIndex -> insertIfAbsent k (index,rIndex)) tmpC (Map.findWithDefault Map.empty x b)

isCorrect :: Exp -> [Exp] -> [Exp] -> Bool
isCorrect statement context proof = (proof /= []) && (last proof == statement) && (helper proof 1 Map.empty Map.empty Map.empty)
  where
    hypoSet :: Set.Set Exp
    hypoSet = Set.fromList context

    helper :: [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> Bool
    helper [] _ _ _ _ = True
    helper (x:xs) index a b c = let (newA,newB,newC) = nextState x index a b c in
      if Set.member x hypoSet || getAxiom x /= Nothing || Map.member x c then
        helper xs (succ index) newA newB newC
      else False

minimize :: Exp -> [Exp] -> [Exp] -> [Exp]
minimize statement context proof = fst $ helper proof 1 Map.empty Map.empty Map.empty
  where
    hypoSet :: Set.Set Exp
    hypoSet = Set.fromList context

    helper :: [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> ([Exp],Set.Set Int)
    helper [x] _ _ _ c = ([x],
      (if Set.member x hypoSet || getAxiom x /= Nothing then
        Set.empty
      else
        case Map.lookup x c of
          Just (a,b) -> Set.insert a $ Set.singleton b))
    helper (x:xs) index a b c = let
      (newA,newB,newC) = nextState x index a b c
      (goods,used) = helper xs (succ index) newA newB newC
      newUsed =
        if Set.member x hypoSet || getAxiom x /= Nothing then
          used
        else
          case Map.lookup x c of
            Just (a,b) -> Set.insert a $ Set.insert b used
      in if Set.member index used then (x:goods,newUsed) else (goods,used)

annotate :: Exp -> [Exp] -> [Exp] -> [String]
annotate statement context proof = helper proof 1 Map.empty Map.empty Map.empty
  where
    hypoMap :: Map.Map Exp Int
    hypoMap = Map.fromList (zip context [1..])

    helper :: [Exp] -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> [String]
    helper [] _ _ _ _ = []
    helper (x:xs) index a b c = let (newA,newB,newC) = nextState x index a b c in
      (fn x index c) : (helper xs (succ index) newA newB newC)

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
foo contents =
  if isCorrect statement context proof
    then (show $ Stmt statement context) : (annotate statement context . minimize statement context $ proof)
    else ["Proof is incorrect"]
  where
    (contextS:statementS:[]) = Split.splitOn "|-" (head contents)
    statement = parse statementS
    context = map parse (filter (\s -> any (not . isSpace) s) (Split.splitOn "," contextS))
    proof = map parse . tail $ contents

main :: IO ()
main = do
    contents <- getContents;
    mapM_ putStrLn (foo . lines $ contents);