module C.Schemes (checkSchemes) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set
  
  import C.Common

  schemes :: [Exp]
  schemes = map parse [
    "A->B->A",
    "(A->B)->(A->B->C)->A->C",
    "A&B->A",
    "A&B->B",
    "A->B->A&B",
    "A->A|B",
    "B->A|B",
    "(A->C)->(B->C)->A|B->C",
    "(A->B)->(A->!B)->!A",
    "!!A->A"]

  cSchemes :: Exp -> Errors -> Either Annotation Errors
  cSchemes x e = case helper 1 schemes of
    Just i  -> Left $ "Ax. sch. " ++ (show i)
    Nothing -> Right e
    where
      helper :: Int -> [Exp] -> Maybe Int
      helper i [] = Nothing
      helper i (a:as) = if fits a x then Just i else helper (i + 1) as

      fits :: Exp -> Exp -> Bool
      fits a x = case morphism a x of
        Nothing -> False
        _ -> True

  qSchemes :: Exp -> Errors -> Either Annotation Errors
  qSchemes x e = return e >>= scheme11 >>= scheme12
    where
      scheme11 :: Errors -> Either Annotation Errors
      scheme11 = case x of
        EImpl (EForall (EVar c) a) b -> common a c b "Ax. sch. 11"
        _ -> Right

      scheme12 :: Errors -> Either Annotation Errors
      scheme12 = case x of
        EImpl b (EExists (EVar c) a) -> common a c b "Ax. sch. 12"
        _ -> Right

      common :: Exp -> Char -> Exp -> String -> Errors -> Either Annotation Errors
      common a c b msg e = case isReplacedWith c (const True) a b of
        Good t ->
          if isReplacementFree t c a then Left msg
          else Right $ Map.insertWith (flip const) 2 ("variable " ++ c : " is not free for term " ++ (show t) ++ " in ?@-axiom.") e
        Ugly   -> Left msg
        Bad    -> Right e

  iScheme :: Exp -> Errors -> Either Annotation Errors
  iScheme (EImpl (EConj a (EForall (EVar x) (EImpl b c))) d) e
    | b /= d = Right e
    | b == d =
      if (isReplacedWith x (== EZero) b a) /= Bad && (isReplacedWith x (== EInc (EVar x)) b c) /= Bad then
        Left "Ax. sch. A9"
      else Right e
  iScheme _ e = Right e

  checkSchemes :: Exp -> Errors -> Either Annotation Errors
  checkSchemes x e = return e >>= cSchemes x >>= qSchemes x >>= iScheme x
