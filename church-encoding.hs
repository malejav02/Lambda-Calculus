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

successor:: Natural -> Expr 
successor n = betared (App  (Lam "n"(App (Var "f")(Var "x"))) 
     (App (Var"f")(App (Var"n")(App (Var"f")(Var "x")))))

add:: Natural -> Natural -> Expr 
add m n = betared 
    (App
    (Lam "m" (App (Var "n") (App (Var "f")(Var "x")) ))
    (App (Var "m")(App (Var "f")(App (Var "n")(App(Var "f")(Var "x")))))
    )

mult:: Natural->Natural->Expr 
mult m n= betared(
    App 
     (Lam "m" (App (Var "n") (App (Var "f")(Var "x")) ))
    (App(App(Var "m")(App (Var "n")(Var "f")))(Var "x")))


exp:: Natural -> Natural -> Expr 
exp m n= betared(
    App
    (Lam "m" (App (Var "n") (App (Var "f")(Var "x")) ))
    (App (App (Var "n")(Var "m"))(App (Var "f")(Var "x"))))


pred:: Natural->Expr 
pred n = betared(
    App
    (Lam "n"(App (Var "f")(Var "x")))
   (App 
   
    (App
    (
        App (Var "g")(Lam "g" 
        (Lam "h" (App(Var "h")
        (App(Var "g")(Var "f")))))
    )
    (Lam "u" (Var "x")))
    (Lam "u" (Var "u"))))