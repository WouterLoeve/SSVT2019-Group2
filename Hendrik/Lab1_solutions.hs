module Lab1_solutions where
import Lab1
import Data.List
import Test.QuickCheck

-- Helpers --
genPositiveInts :: Gen Int
genPositiveInts = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

-- Part 1 / Time: 15m --
sumSquares :: Int -> Int
sumSquares n = sum (map (\ a -> a ^ 2) [1..n])

sumSquares' :: Int -> Int
sumSquares' n = (n * (n + 1) * (2 * n + 1)) `div` 6

testSumSquares :: Int -> Bool
testSumSquares n = sumSquares n == sumSquares' n

sumCubes :: Int -> Int
sumCubes n = sum (map (\ a -> a ^ 3) [1..n])

sumCubes' :: Int -> Int
sumCubes' n = ((n * (n + 1)) `div` 2) ^ 2

testSumCubes :: Int -> Bool
testSumCubes n = sumCubes n == sumCubes' n

-- Part 2 / Time: 24m --
testPowersetCardinality :: Int -> Bool
testPowersetCardinality n = length (subsequences [1..n]) == 2 ^ n

{-
The property is hard to test as only a limited range of numbers
is practically able to be tested due to the running time. Partly
due to this we are only able to test a very finite amount of the
output of the funtion against the equation. It is thus not really
possible to test the correctness of the equation as a whole.
-}

-- Part 3 / Time: 10m --
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fac :: Int -> Int
fac 0 = 1
fac n = foldr (*) 1 [1..n]

testPermsCardinality :: Int -> Bool
testPermsCardinality n = length (perms [1..n]) == fac n

{-
This is essentially the same as the previous part with a
different function.
-}

-- Part 4 / Time: 15m --
reversiblePrimes :: [Integer]
reversiblePrimes = takeWhile (< 10000) (filter (\ n -> prime (reversal n)) primes)

{-
There is no way unless one finds a library implementation which is
assumed to be correct.
-}

-- Part 5 / Time: 15m --
sum101Primes :: Int -> Integer
sum101Primes n = sum (take 101 (drop n primes))

smallestConsecutivePrime :: Integer
smallestConsecutivePrime = head (filter prime (map sum101Primes [1..]))

{-
There doesn't seem to be a way to test this as the implementation
of the test can at best be as accurate as the original
(i.e. a different implementation or by hand).
-}

-- Part 6 / Time: 20m --
takeNPrimes :: Int -> [Integer]
takeNPrimes n = take n primes

productPlusOneNotPrime :: [Integer] -> Bool
productPlusOneNotPrime l = not (prime (product (l) + 1))

smallestCounterExample :: [Integer]
smallestCounterExample = head (filter productPlusOneNotPrime (map takeNPrimes [1..]))

-- Part 7 / Time: 2h --
digits :: Integer -> [Integer]
digits = map (read . return) . show

luhndouble :: [Integer] -> [Integer]
luhndouble l = [if odd (snd x) then fst x else sum (digits (fst x * 2)) | x <- zip (reverse l) [1..]]

luhn :: Integer -> Bool
luhn n = sum (luhndouble (digits n)) `mod` 10 == 0

checkIIN :: [Integer] -> [Integer] -> Bool
checkIIN prefixes list = any ((flip isPrefixOf) list) (map digits prefixes)

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n
    && length l == 15 
    && checkIIN [34, 37] l 
    where l = digits n

isMaster :: Integer -> Bool
isMaster n = luhn n
    && length l == 16
    && checkIIN ([2221 .. 2720] ++ [51 .. 55]) l 
    where l = digits n

isVisa :: Integer -> Bool
isVisa n = luhn n
    && length l == 16 
    && checkIIN [4] l 
    where l = digits n

testCC :: Bool
testCC = all isAmericanExpress amex
      && all isMaster master
      && all isVisa visa
      && not (any isAmericanExpress master)
      && not (any isAmericanExpress visa)
      && not (any isMaster amex)
      && not (any isMaster visa)
      && not (any isVisa amex)
      && not (any isVisa master)
      && not (any isAmericanExpress other)
      && not (any isMaster other)
      && not (any isVisa other)
        where amex = [1]
              master = [1]
              visa = [1]
              other = [1]
    
-- Part 8 / Time: N/A --
accuses :: Boy -> Boy -> Bool
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack Carl = True
accuses Jack Matthew = True
accuses Arnold Jack = True
accuses Arnold Matthew = True
accuses Jack Matthew = True

accusers :: Boy -> [Boy]


guilty :: [Boy]


honest :: [Boy]