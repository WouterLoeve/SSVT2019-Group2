module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import System.Random
import Lecture5
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
exM m e n = 0
{-
 - Exercise 2
 - Time: 0 min
 -
 - The set of natural numbers can be split into primes and non-primes
 - All natural numbers are divisible by 1 and itself
 - All prime numbers are ONLY divisible by 1 and itself
 - All composite numbers are divisible by MORE than 1 and itself
 - Because all prime numbers are ONLY divisible by 1 and itself, and all natural numbers
 - are divisible by 1 and itself, all non-primes must be divisible by MORE than 1 and itself
 - Therefore all non-primes are composite numbers
-}
composites :: [Integer]
composites = filter (\n -> not (prime n)) [2..]

{-
 - Exercise 3
 - Time: 0 min
 -}


recursivePrimeTestF :: Int -> [Integer] -> IO Integer
recursivePrimeTestF k (v:vs) = do
    numPassTest <- primeTestsF k v
    if numPassTest then return v else recursivePrimeTestF k vs
    
    
test_compositePrimeTestF = do
    print "composite prime test K=1"
    recursivePrimeTestF 1 composites
    print "composite prime test K=2"
    recursivePrimeTestF 2 composites
    print "composite prime test K=3"
    recursivePrimeTestF 3 composites


{-
 - Exercise 4
 - Time: 0 min
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

test_carmichaelPrimeTestF :: IO ()
test_carmichaelPrimeTestF = do
    print "carmichael prime test K=1"
    recursivePrimeTestF 1 carmichael >>= print
    print "carmichael prime test K=2"
    recursivePrimeTestF 2 carmichael >>= print
    print "carmichael prime test K=3"
    recursivePrimeTestF 3 carmichael >>= print

{-
 - Exercise 5
 - Time: 0 min
-}

millerRabin = [2^p -1 | p <- primes, prime (2^p -1)]

