import ChurchEncoding 
import LambdaCalculus
import Numeric.Natural ( Natural ) 
import Test.QuickCheck
    ( Arbitrary(arbitrary)
    , quickCheck
    ,shrink
    ,shrinkIntegral
    ,maxSuccess
    ,quickCheckWith
    ,stdArgs
    ,maxSize
    ,arbitrarySizedNatural
    )
instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink  = shrinkIntegral

-- Test para operacion sucesor, recibe un natural
-- y devuelve un booleano que indica si la funcion 
-- creada está buena.
-- utiliza la función alphaEq que nos permite saber
-- si dos Expr son iguales
-- igualamos la funcion succesor pasandole como 
--argumento el número m con church encoding, con
-- la funcion church encoding pasandole argumento m+1

testSucc :: Natural -> Bool
testSucc m =alphaEq (successor (church m)) (church (m+1))

testAdd :: Natural -> Natural -> Bool
testAdd m n  = alphaEq (add (church m ) (
                church n )) ( church (m+n))

testMult :: Natural -> Natural -> Bool
testMult m n  =alphaEq( mult (church m ) (church n )) (
                church (m*n))

testExp :: Natural -> Natural -> Bool
testExp m n  = alphaEq(ChurchEncoding.exp (church m ) (
                church n )) ( church (m^n))

testPred :: Natural  -> Bool
testPred n = alphaEq(ChurchEncoding.pred (
            church (n+1) ) )(betared( church n))

 
test:: Natural -> Natural -> Bool 
test m n= (testSucc m)&& (testSucc n) && (testAdd m n) && (
          testMult m n)&& (testPred m) && (testPred n)

main:: IO()
main = do 
    quickCheck $ test 
    (quickCheckWith stdArgs{maxSuccess = 100, maxSize=9}) $ testExp 

