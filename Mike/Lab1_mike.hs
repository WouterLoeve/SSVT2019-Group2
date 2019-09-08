
module Lab1_mike where
import Lab1
import Data.List
import Test.QuickCheck

genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

-- 20 min --
sumSquares' :: Int -> Int
sumSquares' n = sum [ k^2 | k <- [1..n] ]

sumSquares :: Int -> Int
sumSquares n = quot (n*(n + 1)*(2*n + 1)) 6

testSumSquares :: Int -> Bool
testSumSquares n = sumSquares' n == sumSquares n

-- 10 min --
power :: Int -> Int -> Int
power x y = x^y

sumCubes' :: Int -> Int
sumCubes' n = sum [ k^3 | k <- [1..n] ]

sumCubes :: Int-> Int
sumCubes n = power (quot (n*(n + 1)) 2) 2

testSumCubes :: Int -> Bool
testSumCubes n = sumCubes' n == sumCubes n

-- 10 min --
testSetLength :: Int -> Bool
testSetLength n = length (subsequences [1..n]) == 2^n

-- 15 min --
fac :: Int -> Int
fac 0 = 1
fac n = fac (n - 1) * n

testPermsAmount :: Int -> Bool
testPermsAmount n = length (permutations [1..n]) == fac n

--  --
findPrimes :: [Integer]
findPrimes = filter (\ n -> prime n && (prime (reversal n))) [1..10000]

-- --
-- refutePrimes :: [Integer]
-- refutePrimes = takeWhile (\ xs -> prime ((product xs) + 1)) primes

-- --

-- digits :: Integer -> [Integer]
-- digits = map (read . (:[])) . show
--
-- luhn2 ::[Integer] -> [Integer]
-- luhn2 ns =
--
-- luhn1 :: [Integer] -> [Integer]
-- luhn1 ns = (luhn2 ns) ++ (luhn3 ns)
--
-- luhn :: Integer -> [Integer]
-- luhn n = (digits (reversal n))

-- --
accuses :: Boy -> Boy -> Bool
accuses x y
    | x == Peter && y == Matthew = True
    | x == Peter && y == Jack = True
    | x == Jack && y == Matthew = True
    | x == Jack && y == Peter = True
    | x == Carl && y == Arnold = True
    | otherwise = False

accusers :: Boy -> [Boy]
accusers x
    | x == Matthew = [Peter]
    | x == Jack = [Peter]
