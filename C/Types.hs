module C.Types where
  import qualified Data.Map.Strict as Map

  data Token
    = TLower Char
    | TUpper Char
    | TZero
    | TSym String
    deriving (Eq,Show)

  data Exp
    = EVar Char
    | EPred Char
    | EZero
    | EInc Exp
    | ENeg Exp
    | EHead Exp
    | EImpl Exp Exp
    | EDisj Exp Exp
    | EConj Exp Exp
    | ESum Exp Exp
    | EMul Exp Exp
    | EEquals Exp Exp
    | EForall Exp Exp
    | EExists Exp Exp
    deriving (Eq,Ord)

  type Errors = Map.Map Int String
  type Annotation = String

  showBinOp op l r = "(" ++ (show l) ++ op ++ (show r) ++ ")"

  instance Show Exp where
    show (EHead x) = "|-" ++ (show x)
    show (EImpl l r) = showBinOp "->" l r
    show (EDisj l r) = showBinOp "|" l r
    show (EConj l r) = showBinOp "&" l r
    show (EEquals l r) = showBinOp "=" l r
    show (ESum l r) = showBinOp "+" l r
    show (EMul l r) = showBinOp "*" l r
    show (ENeg x) = "(!" ++ (show x) ++ ")"
    show (EForall v p) = "(@" ++ (show v) ++ "." ++ (show p) ++ ")"
    show (EExists v p) = "(?" ++ (show v) ++ "." ++ (show p) ++ ")"
    show (EVar c) = [c]
    show (EPred c) = [c]
    show (EZero) = "0"
    show (EInc x) = (show x) ++ "'"
