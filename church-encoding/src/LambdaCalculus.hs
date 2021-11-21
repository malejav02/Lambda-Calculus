module LambdaCalculus where
import Numeric.Natural (Natural)
import Data.List ( (\\), union )

-- declaramos el tripo de dato que son los simbolos 
type Sym = String

-- declaramos los lambda terminos
data Expr
        = Var Sym
        | App Expr Expr
        | Lam Sym Expr
        deriving (Eq, Read, Show)
        
-- declaramos las variables libres 
freeVars :: Expr -> [Sym] 
freeVars (Var s) = [s] 
freeVars (App f a) = freeVars f `union` freeVars a 
freeVars (Lam i e) = freeVars e \\ [i]

-- (alpha conversion)
--subst reemplaza todas las apariciones libres de v
-- por b en dentro de x 
subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i e) =
            if v == i then
                Lam i e
            else if i `elem` fvx then
                let i' = cloneSym e i
                    e' = substVar i i' e
                in  Lam i' (sub e')
            else
                Lam i (sub e)
        fvx = freeVars x
        cloneSym e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e
substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

-- Comparar expresiones modulo alpha conversiones
alphaEq :: Expr -> Expr -> Bool 
alphaEq (Var v) (Var v1) = v == v1 
alphaEq (App f a) (App f1 a1) = alphaEq f f1 && alphaEq a a1 
alphaEq (Lam s e ) (Lam s1 e1) = alphaEq e (substVar s1 s e1) 
alphaEq _ _ = False

-- reducción normal (beta reduccion)
betared :: Expr -> Expr
betared ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) [] = Lam s (betared e)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map betared as)


-- verificamos sí las beta reducciones son iguales
betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (betared e1) (betared e2)

