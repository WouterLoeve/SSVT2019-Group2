module Exercise1 where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import Control.Conditional
import SetOrd
import System.Random
import Debug.Trace

{-
 - Exercise 1
 - Time: 120 min
-}
exM :: Integer -> Integer -> Integer -> Integer
exM g c n | n == 1 = 0
          | otherwise = exM' g c n 1

exM' g 0 n t = t
exM' g c n t = exM' g (c-1) n ans
    where ans = (g * t) `mod` n

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 g c n | n == 1 = 0
           | otherwise = foldl (\x y -> x * y `mod` n) 1 [g `mod` n | x <- [0..(c-1)]]
        
power :: Integer -> Integer -> Integer -> Integer
power a b c = a^b `mod` c

{-
 - Checks if the answer corresponds to the normal power function x^y `mod` n
-}
prop_checkPower :: [Integer] -> Bool
prop_checkPower [a, b, c] = exM a b c == power a b c

{-
 - Check whether the end result is actually smaller than the modulus
-}
prop_checkPowerMod :: [Integer] -> Bool
prop_checkPowerMod [a, b, c] = exM a b c < c

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

testExm :: IO ()
testExm = do
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPower
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPowerMod