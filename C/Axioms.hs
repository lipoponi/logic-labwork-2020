module C.Axioms where
  import C.Common

  axioms :: [(Exp,Int)]
  axioms = (`zip` [1..]) $ map parse [
    "a=b->a=c->b=c",
    "a=b->a'=b'",
    "a'=b'->a=b",
    "!a'=0",
    "a+0=a",
    "a+b'=(a+b)'",
    "a*0=0",
    "a*b'=a*b+a"]

  checkAxioms :: Exp -> Errors -> Either Annotation Errors
  checkAxioms x e = case lookup x axioms of
    Just i  -> Left $ "Ax. A" ++ (show i)
    Nothing -> Right e
