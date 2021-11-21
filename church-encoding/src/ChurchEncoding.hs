module ChurchEncoding where
import Numeric.Natural (Natural)
import LambdaCalculus 

-- internalnatural genera las f°^n x correspondientes a cada natural n

internalchurch:: Natural-> Expr 
internalchurch n = 
    if n == 0 then Var "x"
    else App (Var "x")(internalchurch (n-1))

-- esta funcion nos mantiene la estructura 
-- base lambda f.lambda x adicionando la estructura
-- f°^n x que se obtiene de la funciín internal natural
church :: Natural->Expr 
church n = Lam "f" (Lam "x" (internalchurch n))

successor:: Expr -> Expr 
successor n =Lam "n" (Lam "f" (Lam "x"(App(Var "f")(
    App(App(Var "n")(Var "f"))(Var "x")))))

add:: Expr -> Expr -> Expr 
add m n =Lam "m" (Lam "n" (Lam "f" (Lam "x" (
    App (App (Var "m")(Var "f"))
    (App (App (Var "n")(Var "f"))(Var "x"))))))

mult:: Expr->Expr->Expr 
mult m n=
    Lam "m" (Lam "n" (Lam "f" (Lam "x" 
    (App (App (Var "m")(App (Var "n")(Var "f"))) (Var "x")))))
exp:: Expr ->Expr -> Expr 
exp m n= 
  Lam "m" (Lam "n" (Lam "f" (Lam "x" 
  (App (App (App (Var "n")(Var "m"))(Var "f"))(Var "x")))))

pred:: Expr->Expr 
pred n = Lam "n" (Lam "f" (Lam "x" (
    App (
        App (
            App (Var "n")(Lam "g" (Lam "h" (App(Var "h")(App (Var "g")(Var "h")))))
        )(Lam "u" (Var "x"))
    )(Lam "u" (Var "u")))))
