module LambdaCalculus where
import Numeric.Natural (Natural)

type Sym = String

data Expr
        = Var Sym
        | App Expr Expr
        | Lam Sym Expr
        deriving (Eq, Read, Show)
 
