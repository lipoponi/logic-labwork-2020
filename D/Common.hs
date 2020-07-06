module D.Common (module D.Common, module D.Types, module D.Parser) where
  import qualified Data.List as List
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  import Control.Monad (foldM)

  import D.Parser
  import D.Types
  import D.Schemes

  extractVars :: Exp -> [Exp]
  extractVars x = Set.toList $ go x
    where
      go :: Exp -> Set.Set Exp
      go x@(EVar name)  = Set.singleton x
      go x@(ENeg inner) = go inner
      go x@(EConj l r)  = go l `Set.union` go r
      go x@(EDisj l r)  = go l `Set.union` go r
      go x@(EImpl l r)  = go l `Set.union` go r

  eval :: Map.Map Exp Bool -> Exp -> Bool
  eval p x = go x
    where
      go :: Exp -> Bool
      go x@(EVar name)  = case Map.lookup x p of Just v -> v
      go x@(ENeg inner) = not (go inner)
      go x@(EConj l r)  = go l && go r
      go x@(EDisj l r)  = go l || go r
      go x@(EImpl l r)  = not (go l) || go r

  structuredEval :: Map.Map Exp Bool -> Exp -> VExp
  structuredEval p x = go x
    where
      binary :: (Bool -> Bool -> Bool) -> (Bool -> VExp -> VExp -> VExp) -> Exp -> Exp -> VExp
      binary fn cons l r = let (lv,rv) = (go l,go r) in cons (value lv `fn` value rv) lv rv

      go :: Exp -> VExp
      go x@(EVar name)  = case Map.lookup x p of Just v -> VVar v name
      go x@(ENeg inner) = let iv = go inner in VNeg (not $ value iv) iv
      go x@(EConj l r)  = binary (&&) VConj l r
      go x@(EDisj l r)  = binary (||) VDisj l r
      go x@(EImpl l r)  = binary (\a b -> not a || b) VImpl l r

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

  nextSA :: Exp -> Set.Set Exp -> Set.Set Exp
  nextSA = Set.insert

  nextSB :: Exp -> Map.Map Exp (Set.Set Exp) -> Map.Map Exp (Set.Set Exp)
  nextSB (EImpl l r) sb =
    if Map.member l sb then
      Map.adjust (Set.insert r) l sb
    else
      Map.insert l (Set.singleton r) sb
  nextSB _ sb = sb

  nextSC :: Exp -> Set.Set Exp -> Map.Map Exp (Set.Set Exp) -> Map.Map Exp (Exp,Exp) -> Map.Map Exp (Exp,Exp)
  nextSC x sa sb sc = Set.foldl' (\acc r -> Map.insert r (x,EImpl x r) acc) tmpSC (Map.findWithDefault Set.empty x sb)
    where
      tmpSC = case x of
        EImpl l r -> if Set.member l sa then Map.insert r (l,x) sc else sc
        _ -> sc

  unhypothesize :: Exp -> Set.Set Exp -> [Exp] -> [Exp]
  unhypothesize h hypos proof = go proof Set.empty Map.empty Map.empty
    where
      caseValid :: Exp -> [Exp]
      caseValid x = [
          (EImpl x (EImpl h x)),
          x,
          (EImpl h x)
        ]
      caseHypo :: [Exp]
      caseHypo = [
          (EImpl h (EImpl h h)),
          (EImpl (EImpl h (EImpl h h)) (EImpl (EImpl h (EImpl (EImpl h h) h)) (EImpl h h))),
          (EImpl (EImpl h (EImpl (EImpl h h) h)) (EImpl h h)),
          (EImpl h (EImpl (EImpl h h) h)),
          (EImpl h h)
        ]
      caseMp :: Exp -> Map.Map Exp (Exp,Exp) -> [Exp]
      caseMp x sc = case Map.lookup x sc of
        Just (j,k) -> [
            (EImpl (EImpl h j) (EImpl (EImpl h (EImpl j x)) (EImpl h x))),
            (EImpl (EImpl h (EImpl j x)) (EImpl h x)),
            (EImpl h x)
          ]
        _          -> undefined

      go :: [Exp] -> Set.Set Exp -> Map.Map Exp (Set.Set Exp) -> Map.Map Exp (Exp,Exp) -> [Exp]
      go [] _ _ _ = []
      go (x:xs) sa sb sc = (++ go xs newSA newSB newSC) $
        if x == h then caseHypo
        else if Set.member x hypos || isScheme x then caseValid x
        else caseMp x sc
        where
          newSA = nextSA x sa
          newSB = nextSB x sb
          newSC = nextSC x sa sb sc
