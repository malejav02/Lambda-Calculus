-- Implementation of the Testing to check if the Church encoding
-- implementation is correct.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2


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


-- The testSucc function receives a natural number and returns a boolean value.
-- This function checks if the successor function is correct. It applies the 
-- alphaEq function to know if two expressions are equal. The expressions to 
-- compare are the successor of a Church numeral calculated with the successor 
-- function and the Church numeral plus one calculated with the church function.
-- If they are equal, the function returns true; otherwise, returns false.
testSucc :: Natural -> Bool
testSucc m =alphaEq (successor (church m)) (church (m+1))


-- The testAdd function receives two natural numbers and returns a boolean 
-- value.
-- This function checks if the add function is correct. It applies the alphaEq
-- function to know if two expressions are equal. The expressions to compare
-- are the addition of two Church numerals calculated with the add function
-- and the Church numeral obtained by the sum of the two initial naturals.
-- If they are equal, the function returns true; otherwise, returns false.
testAdd :: Natural -> Natural -> Bool
testAdd m n  = alphaEq (add (church m ) (
                church n )) (church (m+n))


-- The testMult function receives two natural numbers and returns a boolean 
-- value.
-- This function checks if the mult function is correct. It applies the alphaEq
-- function to know if two expressions are equal. The expressions to compare
-- are the multiplication of two Church numerals calculated with the mult 
-- function and the Church numeral obtained by the multplication of the two 
-- initial naturals.
-- If they are equal, the function returns true; otherwise, returns false.
testMult :: Natural -> Natural -> Bool
testMult m n  =alphaEq(mult (church m ) (church n )) (
                church (m*n))


-- The testExp function receives two natural numbers and returns a boolean 
-- value.
-- This function checks if the exp function is correct. It applies the alphaEq
-- function to know if two expressions are equal. The expressions to compare
-- are the exponentiation of two Church numerals calculated with the exp 
-- function and the Church numeral obtained by the first number, raised to the
-- second number. 
-- If they are equal, the function returns true; otherwise, returns false.
testExp :: Natural -> Natural -> Bool
testExp m n  = alphaEq(ChurchEncoding.exp (church m ) (
                church n )) (church (m^n))


-- The testPred function receives a natural number and returns a boolean 
-- value.
-- This function checks if the pred function is correct. It applies the alphaEq
-- function to know if two expressions are equal. The expressions to compare
-- are the predeccessor of the natural number plus one, calculated with the 
-- pred function and the β-reduction of the n Church numeral.
-- If they are equal, the function returns true; otherwise, returns false.
testPred :: Natural  -> Bool
testPred n = alphaEq(ChurchEncoding.pred (
            church (n+1) ) )(betared( church n))

 
-- The test function receives two natural numbers and returns a boolean value.
-- This function confirms if all the last functions are correct, checking with
-- the given arguments. 
-- If they are correct, the function returns true; otherwise, returns false.
test:: Natural -> Natural -> Bool 
test m n= (testSucc m)&& (testSucc n) && (testAdd m n) && (
          testMult m n)&& (testPred m) && (testPred n)

          
main:: IO()
main = do 
    quickCheck $ test 
    (quickCheckWith stdArgs{maxSuccess = 100, maxSize=9}) $ testExp 
 
test:: Natural -> Natural -> Bool 
test m n= (testSucc m)&& (testSucc n) && (testAdd m n) && (
          testMult m n)&& (testPred m) && (testPred n)

main:: IO()
main = do 
    quickCheck $ test 
    (quickCheckWith stdArgs{maxSuccess = 100, maxSize=9}) $ testExp 

