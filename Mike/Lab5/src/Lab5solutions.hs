module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
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
 - Time: 50 min
-}

powOfTwo :: Integer -> Integer -> Bool
powOfTwo x y | x == y = True
             | x > y  = False
             | otherwise = powOfTwo (x*2) y

multExp :: Integer -> Integer -> Integer -> Integer -> Integer
multExp x y n z | y /= z = multExp ((x^2) `mod` n) y n (z*2)
              | otherwise = x

exMM :: Integer -> Integer -> Integer -> Integer
exMM x y n = exM' x y n 1

exM' x y n res | powOfTwo 2 y = (res * (multExp (x `mod` n) y n 1) `mod` n)
               | otherwise    = exM' x (y-1) n ((res * (x `mod` n)) `mod` n)

{-
- Exercise 2
- Time: 10 min
-}

composites :: [Integer]
composites = filter (not . prime) [2..]

{-
- Exercise 3
- Time: 50 min
- k = 1: [9,21,28,65,85,105,117,121]
- k = 2: [15,85]
- k = 3: [121,703]
-
- The more you increase k, the more accurate the test will be, since it
- becomes harder to find a composite number that fools the test.
-}

leastComposite1 = filterM (\x -> primeTestsF 1 x) (take 100 composites)
leastComposite2 = filterM (\x -> primeTestsF 2 x) (take 100 composites)
leastComposite3 = filterM (\x -> primeTestsF 3 x) (take 1000 composites)

{-
- Exercise 4
- Time: 25 min
-
- Every Carmichael number will pass the Fermat's primality test even though
- they are not actually primes, and are called Fermat pseudoprimes.
-}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

leastCarmichael1 = filterM (\x -> primeTestsF 1 x) (take 1 carmichael)
leastCarmichael2 = filterM (\x -> primeTestsF 2 x) (take 1 carmichael)

leastCarmichaelMiller1 = filterM (\x -> primeMR 1 x) (take 10 carmichael)

{-
- Exercise 5
- Time: 15 min
-
-}
mersenne = filterM (\p -> primeMR 1 ((2^p) - 1)) (take 8 primes) --[(2^p) - 1 | p <- primes, primeMR 1 ((2^p) - 1)]
