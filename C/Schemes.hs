module C.Schemes where
import C.Common
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

isFree :: Exp -> Char -> Exp -> Bool
isFree a v t = helper a (extractVars t) Set.empty
  where
    helper :: Exp -> Set.Set Char -> Set.Set Char -> Bool
    helper (EVar c) s bound = if c == v then Set.empty == Set.intersection s bound else True
    helper (EInc x) s bound = helper x s bound
    helper (ENeg x) s bound = helper x s bound
    helper (EHead x) s bound = helper x s bound
    helper (EImpl a b) s bound = helper a s bound && helper b s bound
    helper (EDisj a b) s bound = helper a s bound && helper b s bound
    helper (EConj a b) s bound = helper a s bound && helper b s bound
    helper (ESum a b) s bound = helper a s bound && helper b s bound
    helper (EMul a b) s bound = helper a s bound && helper b s bound
    helper (EEquals a b) s bound = helper a s bound && helper b s bound
    helper (EForall (EVar c) x) s bound = helper x s (Set.insert c bound)
    helper (EExists (EVar c) x) s bound = helper x s (Set.insert c bound)
    helper _ _ _ = True

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
    common a c b msg e = case morphismWith a b (\k b -> k == c || EVar k == b) of
      Nothing -> Right e
      Just m  ->
        case Map.lookup c m of
          Nothing -> Left msg
          Just t  ->
            if isFree a c t then Left msg
            else Right $ (2,"variable " ++ c : " is not free for term " ++ (show t) ++ " in ?@-axiom.") : e

iScheme :: Exp -> Errors -> Either Annotation Errors
iScheme (EImpl (EConj a (EForall (EVar x) (EImpl b c))) d) e
  | b /= d = Right e
  | b == d = case morphismWith b a (\k b -> k == x && b == EZero || k /= x && EVar k == b) of
    Nothing -> Right e
    _       -> case morphismWith b c (\k b -> k == x && b == EInc (EVar k) || k /= x && EVar k == b) of
      Nothing -> Right e
      _       -> Left "Ax. sch. A9"
iScheme _ e = Right e

checkSchemes :: Exp -> Errors -> Either Annotation Errors
checkSchemes x e = return e >>= cSchemes x >>= qSchemes x >>= iScheme x
