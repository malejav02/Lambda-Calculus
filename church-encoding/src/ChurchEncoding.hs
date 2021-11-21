module ChurchEncoding where
import Numeric.Natural (Natural)
import LambdaCalculus 

-- internalnatural genera las f°^n x correspondientes a cada natural n

internalchurch:: Natural-> Expr 
internalchurch 0 = Var "x"
internalchurch n= App (Var "f")(internalchurch (n-1))

-- esta funcion nos mantiene la estructura 
-- base lambda f.lambda x adicionando la estructura
-- f°^n x que se obtiene de la funciín internal natural
church :: Natural->Expr 
church n = Lam "f" (Lam "x" (internalchurch n))

successor:: Expr -> Expr 
successor n =betared ( App (Lam "n" (Lam "f" (Lam "x"(App(Var "f")(
    App(App(Var "n")(Var "f"))(Var "x")))))) n)

add:: Expr -> Expr -> Expr 
add m n = 
    betared (
    App (
    App (
    
    Lam "m" (Lam "n" (Lam "f" (Lam "x" (
    App (App (Var "m")(Var "f"))
    (App (App (Var "n")(Var "f"))(Var "x")))))))(m))(n))


mult:: Expr->Expr->Expr 
mult m n=
    betared(
    App (
    App (
    Lam "m" (Lam "n" (Lam "f" (Lam "x" 
    (App (App (Var "m")(App (Var "n")(Var "f"))) (Var "x"))))))(m))(n))

exp:: Expr ->Expr -> Expr 
exp m n= 
    betared(
    App(
   App ( 
  Lam "m" (Lam "n" (Lam "f" (Lam "x" 
  (App (App (App (Var "n")(Var "m"))(Var "f"))(Var "x"))))))(m))(n))

pred:: Expr->Expr 
pred n=
     if n == church 0 then betared (church 0)
else
    betared(
    App(
    Lam "n" (Lam "f" (Lam "x" (
    App (
        App (
            App (Var "n")(Lam "g" (Lam "h" (App(Var "h")(App (Var "g")(Var "f")))))
        )(Lam "u" (Var "x"))
    )(Lam "u" (Var "u"))))))n)


