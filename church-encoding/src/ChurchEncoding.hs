-- Implementation of the Church numerals and the arithmetic operations of
-- addition, multiplication, exponentiation and predeccesor.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2


module ChurchEncoding where
import Numeric.Natural (Natural)
import LambdaCalculus 


-- The internalnatural function recursively generates the application f^n x 
-- corresponding to each natural n.
-- This function receives a natural number and returns a type of expression
internalchurch:: Natural-> Expr 
internalchurch 0 = Var "x"
internalchurch n= App (Var "f")(internalchurch (n-1))


-- The church function preserve the base structure λfx adding the structure 
-- f^n x, given by the function internalchurch.
-- This function receives a natural number and returns a type of expression.
church :: Natural->Expr 
church n = Lam "f" (Lam "x" (internalchurch n))


-- The successor function receives an expression and returns another 
-- expression.
-- This function applies a β-reduction to obtein a natural number defined 
-- from zero and succesor.
successor:: Expr -> Expr 
successor n =betared ( 
    App (
    Lam "n" (
    Lam "f" (
    Lam "x"(
    App(
        Var "f")(
    App(
        App(Var "n")(Var "f"))(
            Var "x")))))) 
            n)


-- The add function receives two expressions and returns an expression.
-- This function applies a β-reduction to sum two church numerals. It preserve
-- the addition structure, that is λmnfx.mf(nfx). 
add:: Expr -> Expr -> Expr 
add m n = 
    betared (
    App (
    App (
    Lam "m" (
        Lam "n" (
            Lam "f" (
                Lam "x" (
    App (
        App (Var "m")(Var "f"))
    (App (
        App (Var "n")(Var "f"))(
            Var "x")))))))(m))(n))


-- The mult function receives two expressions and returns an expression.
-- This function applies a β-reduction to multiply two church numerals. It 
-- preserve the multiply structure, that is λmnfx.m(nf)x.
mult:: Expr->Expr->Expr 
mult m n=
    betared(
    App (
    App (
    Lam "m" (
        Lam "n" (
            Lam "f" (
                Lam "x" 
    (App (
        App (
            Var "m")(
                App (Var "n")(Var "f"))) (
                    Var "x"))))))(m))(n))


-- The exp function receives two expressions and returns an expression.
-- This function applies a β-reduction to do the first expression to the power 
-- of the second expression. It preserve the exponentiation structure, that is 
-- λmnfx.(nm)fx.
exp:: Expr ->Expr -> Expr 
exp m n= 
    betared(
    App(
   App ( 
  Lam "m" (
      Lam "n" (
          Lam "f" (
              Lam "x" 
  (App (
      App (
          App (Var "n")(Var "m"))(
              Var "f"))(Var "x"))))))(m))(n))


-- This function receives an expression and returns an expression.
-- The pred function calculates the predeccesor of a Church numeral. If the
-- Church numeral represents zero, then applies a β-reduction to the number.
-- Then, if the Church numeral is different, the function applis a β-reduction
-- to obtein the predeccesor of n. It preserves the predeccesor structure, that
-- is λnfx.n(λgh.h(gf))(λu.x)(λu.u). 
pred:: Expr->Expr 
pred n=
     if n == church 0 then betared (church 0)
else
    betared(
    App(
    Lam "n" (
        Lam "f" (
            Lam "x" (
    App (
        App (
            App (Var "n")(
                Lam "g" (
                    Lam "h" (
                        App(Var "h")(
                            App (Var "g")(
                                Var "f")))))
        )(Lam "u" (Var "x"))
    )(Lam "u" (Var "u"))))))n)

