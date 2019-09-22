module Lab3solutions where
import Data.List
import Data.Char
import Data.Tuple
import SetOrd
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import Lecture3

{- 
 - Exercise 1
 - Time:  min
 - A: 
-}
evlHelper f = (map (\ v -> evl v f) (allVals f))

contradiction :: Form -> Bool
tautology :: Form -> Bool
--entails :: Form -> Form -> Bool
--equiv :: Form -> Form -> Bool

formContradiction = Equiv p (Neg p)
contradiction f = all (==False) (evlHelper f)

formTautology = Equiv p p
tautology f = all (==True) (evlHelper f) 

-- first compare propNames f1 == propNames f2? return False if mismatch?
formEntails = Equiv p p
entails f1 f2 = 
