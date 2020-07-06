module D.Types where
  data Token
    = TSym String
    | TVar String
    deriving (Eq,Show)

  data Statement
    = Statement Exp [Exp]

  data Exp
    = EVar String
    | ENeg Exp
    | EConj Exp Exp
    | EDisj Exp Exp
    | EImpl Exp Exp
    deriving (Eq,Ord)

  data VExp
    = VVar { value :: Bool, name :: String }
    | VNeg { value :: Bool, inner :: VExp }
    | VConj { value :: Bool, left :: VExp, right :: VExp }
    | VDisj { value :: Bool, left :: VExp, right :: VExp }
    | VImpl { value :: Bool, left :: VExp, right :: VExp }

  instance Show Statement where
    show (Statement x [])     = "|- " ++ show x
    show (Statement x (h:[])) = (show h) ++ " " ++ (show $ Statement x [])
    show (Statement x (h:hs)) = (show h) ++ ", " ++ (show $ Statement x hs)

  instance Show Exp where
    show (EVar name) = name
    show (ENeg var@(EVar name)) = "!" ++ (show var)
    show (ENeg inner@(ENeg _)) = "!" ++ (show inner)
    show (ENeg inner) = "!(" ++ (show inner) ++ ")"
    show (EConj l@(EDisj _ _) r@(EDisj _ _)) = "(" ++ (show l) ++ ") & (" ++ (show r) ++ ")"
    show (EConj l@(EDisj _ _) r@(EImpl _ _)) = "(" ++ (show l) ++ ") & (" ++ (show r) ++ ")"
    show (EConj l@(EDisj _ _) r) = "(" ++ (show l) ++ ") & " ++ (show r)
    show (EConj l@(EImpl _ _) r@(EDisj _ _)) = "(" ++ (show l) ++ ") & (" ++ (show r) ++ ")"
    show (EConj l@(EImpl _ _) r@(EImpl _ _)) = "(" ++ (show l) ++ ") & (" ++ (show r) ++ ")"
    show (EConj l@(EImpl _ _) r) = "(" ++ (show l) ++ ") & " ++ (show r)
    show (EConj l r@(EDisj _ _)) = (show l) ++ " & (" ++ (show r) ++ ")"
    show (EConj l r@(EImpl _ _)) = (show l) ++ " & (" ++ (show r) ++ ")"
    show (EConj l r) = (show l) ++ " & " ++ (show r)
    show (EDisj l@(EImpl _ _) r@(EImpl _ _)) = "(" ++ (show l) ++ ") | (" ++ (show r) ++ ")"
    show (EDisj l@(EImpl _ _) r) = "(" ++ (show l) ++ ") | " ++ (show r)
    show (EDisj l r@(EImpl _ _)) = (show l) ++ " | (" ++ (show r) ++ ")"
    show (EDisj l r) = (show l) ++ " | " ++ (show r)
    show (EImpl l@(EImpl _ _) r) = "(" ++ (show l) ++ ") -> " ++ (show r)
    show (EImpl l r) = (show l) ++ " -> " ++ (show r)
