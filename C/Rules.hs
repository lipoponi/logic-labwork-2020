module C.Rules (checkRules) where
  import C.Common

  checkRules :: Exp -> Errors -> Either Annotation Errors
  checkRules _ = return