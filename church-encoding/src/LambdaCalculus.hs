-- Implementation of the λ-calculus using Haskell.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2


module LambdaCalculus where
import Numeric.Natural (Natural)
import Data.List ( (\\), union )

-- The type of data that the symbols are is declared to be String.
type Sym = String

-- λ-termns are declared
data Expr
        = Var Sym
        | App Expr Expr
        | Lam Sym Expr
        deriving (Eq, Read, Show)
        
-- The freeVars function gets the free variables of a λ-term. Free 
-- variables occur in an expression but aren't bound to it.
freeVars :: Expr -> [Sym] 
freeVars (Var s) = [s] 
freeVars (App f a) = freeVars f `union` freeVars a 
freeVars (Lam i e) = freeVars e \\ [i]

-- α conversion.
-- The subst function replaces all free occurrences of v by b within x.
-- In other words, change the name of a bound variable.
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

-- The substVar function substitute one variable for another.
substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

-- The alphaEq function compare expressions module α-conversion.
alphaEq :: Expr -> Expr -> Bool 
alphaEq (Var v) (Var v1) = v == v1 
alphaEq (App f a) (App f1 a1) = alphaEq f f1 && alphaEq a a1 
alphaEq (Lam s e ) (Lam s1 e1) = alphaEq e (substVar s1 s e1) 
alphaEq _ _ = False

-- β-reduction.
-- The betared function reduce an expresion (λx.t)s to the form t[x := s]
betared :: Expr -> Expr
betared ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) [] = Lam s (betared e)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map betared as)


-- The betaEq function compare expressions module β-reduction.
betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (betared e1) (betared e2)
