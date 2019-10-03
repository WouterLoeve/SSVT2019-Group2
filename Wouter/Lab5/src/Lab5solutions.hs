module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import Control.Conditional
import SetOrd
import System.Random
import Lecture5
import Debug.Trace
import Exercise1

{-
 - testRunhelper helps print results
 -}
testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "--- FAIL" else "+++ OK"
    let append = if numCases == numPass then "" else " ("++ show numFail ++" Failed)"
    prepend ++ ", Test '" ++ testName ++ "' Passed " ++ show numPass ++ " out of " ++ show numCases ++ " cases" ++ append

{-
 - Exercise 2
 - Time: 120 min
-}
composites :: [Integer]
composites = [x * y | x <- [2..], y <- [1..]]

{-
 - Exercise 3
 - Time: 120 min
-}


-- test_primeTestsF k = test_primeTestF' k (take 50000 composites) []

-- test_primeTestF' :: Int -> [Integer] -> [Integer] -> [Integer]
test_primeTestF' :: Int -> [Integer] -> [IO Integer] -> [IO Integer]
test_primeTestF' k [] ys = ys
test_primeTestF' k (x:xs) ys = do
                a <- primeTestsF k x
                ysM <- ys
                ifM a (test_primeTestF' k xs (ys >>= \y -> return $ [liftM x, y]))
                    test_primeTestF' k xs ys

-- factors :: Integer -> [Integer]
-- factors n0 = let
--    ps0 = takeWhile (\ m -> m^2 <= n0) primes
--  in factors' n0 ps0 where 
--    factors' 1 _  = []
--    factors' n [] = [n]
--    factors' n (p:ps) 
--     | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
--     | otherwise      =    factors' n ps

-- prime :: Integer -> Bool
-- prime n = factors n == [n]

-- primes :: [Integer]
-- primes = 2 : filter prime [3..]

{-
 - Exercise 4
 - Time: 120 min
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
        k <- [2..], 
        prime (6*k+1), 
        prime (12*k+1), 
        prime (18*k+1) ]

-- test_primeTestsF k = test_primeTestF' k (carmichael []