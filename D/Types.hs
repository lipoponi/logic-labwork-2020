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

  showBinOp op l r = "(" ++ (show l) ++ " " ++ op ++ " " ++ (show r) ++ ")"
  
  instance Show Exp where
    show (EImpl l r) = showBinOp "->" l r
    show (EDisj l r) = showBinOp "|" l r
    show (EConj l r) = showBinOp "&" l r
    show (ENeg inner) = "!" ++ (show inner)
    show (EVar name) = name
