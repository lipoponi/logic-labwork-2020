module C.Common (module C.Common, module C.Parser, module C.Types) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  
  import C.Parser
  import C.Types

  morphism :: Exp -> Exp -> Maybe (Map.Map Char Exp)
  morphism = aliases
    where
      aliases :: Exp -> Exp -> Maybe (Map.Map Char Exp)
      aliases (EVar name) x = Just (Map.singleton name x)
      aliases (EPred name) x = Just (Map.singleton name x)
      aliases EZero EZero = Just Map.empty
      aliases (EInc a) (EInc x) = aliases a x
      aliases (ENeg a) (ENeg x) = aliases a x
      aliases (EHead a) (EHead x) = aliases a x
      aliases (EImpl al ar) (EImpl xl xr) = stepIn al ar xl xr
      aliases (EDisj al ar) (EDisj xl xr) = stepIn al ar xl xr
      aliases (EConj al ar) (EConj xl xr) = stepIn al ar xl xr
      aliases (ESum al ar) (ESum xl xr) = stepIn al ar xl xr
      aliases (EMul al ar) (EMul xl xr) = stepIn al ar xl xr
      aliases (EEquals al ar) (EEquals xl xr) = stepIn al ar xl xr
      aliases (EForall al ar) (EForall xl xr) = stepIn al ar xl xr
      aliases (EExists al ar) (EExists xl xr) = stepIn al ar xl xr
      aliases _ _ = Nothing

      stepIn :: Exp -> Exp -> Exp -> Exp -> Maybe (Map.Map Char Exp)
      stepIn al ar xl xr = case (aliases al xl,aliases ar xr) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just l, Just r) -> merge l r

      merge :: Map.Map Char Exp -> Map.Map Char Exp -> Maybe (Map.Map Char Exp)
      merge l r = let
        fn k !rv = case (Map.lookup k l) of
          (Just lv) -> if lv == rv then Just rv else Nothing
          Nothing -> Just rv
        in case (Map.traverseWithKey fn r) of
          Nothing -> Nothing
          _ -> Just (Map.union l r)

  morphismWith :: Exp -> Exp -> (Char -> Exp -> Bool) -> Maybe (Map.Map Char Exp)
  morphismWith a x p = case morphism a x of
    Nothing -> Nothing
    Just m  -> if Map.foldlWithKey' (\r k b -> r && p k b) True m then Just m else Nothing

  nextSA :: Exp -> Int -> Map.Map Exp Int -> Map.Map Exp Int
  nextSA = Map.insert

  nextSB :: Exp -> Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Map.Map Exp Int)
  nextSB (EImpl l r) index sb = 
    if Map.member l sb then
      Map.adjust (Map.insert r index) l sb
    else
      Map.insert l (Map.singleton r index) sb
  nextSB _ _ sb = sb

  nextSC :: Exp -> Int -> Map.Map Exp Int -> Map.Map Exp (Map.Map Exp Int) -> Map.Map Exp (Int,Int) -> Map.Map Exp (Int,Int)
  nextSC x index sa sb sc = Map.foldlWithKey' (\acc k rIndex -> Map.insertWith max k (index,rIndex) acc) tmpSC (Map.findWithDefault Map.empty x sb)
    where
      tmpSC = case x of
        EImpl l r -> case Map.lookup l sa of
          Just lIndex -> Map.insertWith max r (lIndex,index) sc
          _ -> sc
        _ -> sc

  extractFreeVars :: Exp -> Set.Set Char
  extractFreeVars (EVar c) = Set.singleton c
  extractFreeVars (EInc x) = extractFreeVars x
  extractFreeVars (ENeg x) = extractFreeVars x
  extractFreeVars (EHead x) = extractFreeVars x
  extractFreeVars (EImpl a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (EDisj a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (EConj a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (ESum a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (EMul a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (EEquals a b) = Set.union (extractFreeVars a) (extractFreeVars b)
  extractFreeVars (EForall (EVar c) x) = Set.delete c (extractFreeVars x)
  extractFreeVars (EExists (EVar c) x) = Set.delete c (extractFreeVars x)
  extractFreeVars _ = Set.empty

  isFree :: Char -> Exp -> Bool
  isFree c x = Set.member c (extractFreeVars x)

  isReplacementFree :: Exp -> Char -> Exp -> Bool
  isReplacementFree t v p = helper p (extractFreeVars t) Set.empty
    where
      helper :: Exp -> Set.Set Char -> Set.Set Char -> Bool
      helper (EVar c) tfree pbound = c == v && Set.empty == Set.intersection tfree pbound || c /= v
      helper (EInc p) tfree pbound = helper p tfree pbound
      helper (ENeg p) tfree pbound = helper p tfree pbound
      helper (EHead p) tfree pbound = helper p tfree pbound
      helper (EImpl a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (EDisj a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (EConj a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (ESum a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (EMul a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (EEquals a b) tfree pbound = helper a tfree pbound && helper b tfree pbound
      helper (EForall (EVar c) p) tfree pbound = c == v || helper p tfree (Set.insert c pbound)
      helper (EExists (EVar c) p) tfree pbound = c == v || helper p tfree (Set.insert c pbound)
      helper _ _ _ = True
