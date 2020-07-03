module C.Rules (checkRules) where
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  import C.Common

  modusPonens :: Exp -> Map.Map Exp (Int,Int) -> Errors -> Either Annotation Errors
  modusPonens x sc e = case Map.lookup x sc of
    Nothing    -> Right e
    Just (k,l) -> Left $ "M.P. " ++ (show k) ++ ", " ++ (show l)

  isFree :: Char -> Exp -> Bool
  isFree c x = Set.member c (extractVars x)

  intro :: Exp -> Map.Map Exp Int -> Errors -> Either Annotation Errors
  intro x sa e = return e >>= forall >>= exists
    where
      forall :: Errors -> Either Annotation Errors
      forall = case x of
        (EImpl a (EForall (EVar c) b)) -> common (EImpl a b) a c
        _ -> Right

      exists :: Errors -> Either Annotation Errors
      exists = case x of
        (EImpl (EExists (EVar c) b) a) -> common (EImpl b a) a c
        _ -> Right

      common :: Exp -> Exp -> Char -> Errors -> Either Annotation Errors
      common impl a c e = case Map.lookup impl sa of
        Nothing -> Right e
        Just k  ->
          if isFree c a then
            Right $ (1,"variable " ++ c : " occurs free in ?@-rule.") : e
          else Left $ "?@-intro " ++ (show k)

  checkRules :: Exp -> Map.Map Exp Int -> Map.Map Exp (Int,Int) -> Errors -> Either Annotation Errors
  checkRules x sa sc e = return e >>= modusPonens x sc >>= intro x sa