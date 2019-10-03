module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
import System.Random
import Debug.Trace

{-
 - testRunhelper helps print results
 -}
testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "--- FAIL" else "+++ OK"
    let append = if numCases == numPass then "" else " ("++ show numFail ++" Failed)"
    prepend ++ ", Test '" ++ testName ++ "' Passed " ++ show numPass ++ " out of " ++ show numCases ++ " cases" ++ append
{-
 - Exercise 1
 - Time: 0 min
-}
exM :: Integer -> Integer -> Integer -> Integer
exM g c n = ((exM' g remainder n) `mod` n) * ((exM'' g (c - (2^remainder)) n) `mod` n) `mod` n
    where 
        remainder = floor (logBase 2.0 (fromIntegral c))

exM' :: Integer -> Integer -> Integer -> Integer
exM' g 0 n = 1
exM' g c n = ((g * g) `mod` n) * (exM' g (c-1) n)

exM'' :: Integer -> Integer -> Integer -> Integer
exM'' g 0 n = 1
exM'' g c n = (g `mod` n) * (exM'' g (c-1) n)

power :: Integer -> Integer -> Integer -> Integer
power a b c = a^b `mod` c

prop_checkPower :: [Integer] -> Bool
prop_checkPower [a, b, c] = exM a b c == power a b c

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

temp2 = verboseCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPower
-- exM g c n = foldl ()
--     where
--         remainder = floor (logBase 2.0 (fromIntegral c))