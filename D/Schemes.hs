module D.Schemes where
  import qualified Data.Map.Strict as Map

  import D.Types
  import D.Parser

  schemes :: [Exp]
  schemes = map parse [
      "A->B->A",
      "(A->B)->(A->B->C)->A->C",
      "A->B->A&B",
      "A&B->A",
      "A&B->B",
      "A->A|B",
      "B->A|B",
      "(A->C)->(B->C)->A|B->C",
      "(A->B)->(A->!B)->!A",
      "!!A->A"
    ]

  getScheme :: Exp -> Maybe Int
  getScheme x = helper 1 schemes
    where
      helper :: Int -> [Exp] -> Maybe Int
      helper i [] = Nothing
      helper i (a:as) = if fits a x then Just i else helper (i + 1) as

      fits :: Exp -> Exp -> Bool
      fits a x = case aliases a x of
        Nothing -> False
        _ -> True

      aliases :: Exp -> Exp -> Maybe (Map.Map String Exp)
      aliases (EVar name) x = Just (Map.singleton name x)
      aliases (ENeg ain) (ENeg xin) = aliases ain xin
      aliases (EImpl al ar) (EImpl xl xr) = stepIn al ar xl xr
      aliases (EDisj al ar) (EDisj xl xr) = stepIn al ar xl xr
      aliases (EConj al ar) (EConj xl xr) = stepIn al ar xl xr
      aliases _ _ = Nothing

      stepIn :: Exp -> Exp -> Exp -> Exp -> Maybe (Map.Map String Exp)
      stepIn al ar xl xr = case (aliases al xl,aliases ar xr) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just l, Just r) -> merge l r

      merge :: Map.Map String Exp -> Map.Map String Exp -> Maybe (Map.Map String Exp)
      merge l r = let
        fn k !rv = case (Map.lookup k l) of
          (Just lv) -> if lv == rv then Just rv else Nothing
          Nothing -> Just rv
        in case (Map.traverseWithKey fn r) of
          Nothing -> Nothing
          _ -> Just (Map.union l r)

  isScheme :: Exp -> Bool
  isScheme x = case getScheme x of
    Just _ -> True
    _      -> False
