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
  nextSC x index sa sb sc = Map.foldlWithKey' (\acc k rIndex -> Map.insert k (index,rIndex) acc) tmpSC (Map.findWithDefault Map.empty x sb)
    where
      tmpSC = case x of
        EImpl l r -> case Map.lookup l sa of
          Just lIndex -> Map.insert r (lIndex,index) sc
          _ -> sc
        _ -> sc

  extractVars :: Exp -> Set.Set Char
  extractVars (EVar c) = Set.singleton c
  extractVars (EInc x) = extractVars x
  extractVars (ENeg x) = extractVars x
  extractVars (EHead x) = extractVars x
  extractVars (EImpl a b) = Set.union (extractVars a) (extractVars b)
  extractVars (EDisj a b) = Set.union (extractVars a) (extractVars b)
  extractVars (EConj a b) = Set.union (extractVars a) (extractVars b)
  extractVars (ESum a b) = Set.union (extractVars a) (extractVars b)
  extractVars (EMul a b) = Set.union (extractVars a) (extractVars b)
  extractVars (EEquals a b) = Set.union (extractVars a) (extractVars b)
  extractVars (EForall (EVar c) x) = Set.delete c (extractVars x)
  extractVars (EExists (EVar c) x) = Set.delete c (extractVars x)
  extractVars _ = Set.empty
