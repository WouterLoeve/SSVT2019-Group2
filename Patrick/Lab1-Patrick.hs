import Lab1
import Test.QuickCheck
import Data.List
import Data.Char

-- Help functions
genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

-- Excercise 1: 45 minutes
sumSquares n = sum [ k^2 | k <- [1..n] ]
sumSquares' n = quot (n * (n + 1) * (2*n + 1)) 6
testSumSquares n = let a = abs n in sumSquares' a == sumSquares a

sumPowerThree n = sum [ k^3 | k <- [1..n] ]
sumPowerThree' n = (quot (n * (n + 1)) 2) ^ 2
testSumPowerThree n = let a = abs n in sumPowerThree' a == sumPowerThree a

-- Excercise 2: 15 minutes
supersetCardinality n = length (subsequences [1..n])
supersetCardinality' n = 2 ^ n
testSupersetCardinality n = let a = min n 100 in supersetCardinality a == supersetCardinality' a
{-
 - The property is hard to test. When supplying lists that are too long, it becomes
 - extremely difficult to process the test. This is because generating the superset for
 - a list becomes exponentially more difficult with list length.
 - because there a are infinite numbers and it is impossible to test all of them, it stands to reason
 - that this function actually tests the library implementation and your own function implementation.
 - However, the test does guarantee that the hypothesis holds for the tested numbers
 -}

-- Excercise 3: 20 minutes
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fact 0 = 1
fact n = n * fact (n-1)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
testPerms n = let a = min n 10 in length (perms [1..a]) == fact a
{-
 - The property is hard to test. both the function to create permutations and calculate factorials
 - are recursive in nature. This takes a lot of processing power.
 - Once again this test only tests the library and own implementation, as lists can be infinite and
 - this impossible to test fully. The test does guarantee that the hypothesis holds for the
 - numbers with which it is tested
 -}

-- Excercise 4: 20 minutes
palindromePrimes = takeWhile (<10000) (filter(\ n -> prime (reversal n)) primes)
{-
 - Possibly one could take all numbers in the list, reverse them again and check if they
 - are prime. This would theoretically be the same the original list of primes, thus not
 - very usefull...
 -}
 
-- Excercise 5: 20 minutes
offsetPrime idx = take 101 (drop idx primes)
getPrime idx = if prime (sum (offsetPrime idx)) then sum (offsetPrime idx) else getPrime (idx+1)
{-
 - Here we create a helper function offsetPrime, that takes an offset. 101 primes are taken from the 
 - 'primes' list, starting from this offset.
 - We can use this helper function in the 'getPrime' function, here we check if the 101 given primes
 - sum up to a prime. If not, we recursively call getPrime with the next index. If yes, we have
 - found an offset that produces the 101 primes that sum up to another prime and we return the result.
 -}

-- Excercise 6: 15 minutes
getConjecturePrimes n = take n primes
getConjecture n = (product (getConjecturePrimes n)) + 1
isCounterExample n = not (prime (getConjecture n))
counterExamples = [getConjecturePrimes n | n <- [0..], isCounterExample n]
{-
 - The smallest counterexample is index 30031, or the list [2,3,5,7,11,13].
 -}

-- Exercise 7: 45 minutes
getDigits n = reverse [digitToInt i | i <- show n]
multiplyDigits n = [if i `mod` 2 == 0 then x else x*2 | (i,x) <- zip [0..] (getDigits n)]
luhn :: Integer -> Bool
luhn n = (sum [if i > 9 then (i-9) else i | i <- multiplyDigits n]) `mod` 10 == 0

getNDigits i n = (read (take i (show n)) :: Integer)
nDigitsBetweenRange i n lo hi = let q = getNDigits i n in q >= lo && q <= hi

isAmericanExpress n = luhn n && (length (show n)) == 15 && ((getNDigits 2 n) == 34 || (getNDigits 2 n) == 37)
isMaster n = luhn n && (length (show n)) == 16 && (nDigitsBetweenRange 4 n 2221 2720 || nDigitsBetweenRange 2 n 51 55)
isVisa n = luhn n && (length (show n)) == 16 && (getNDigits 1 n) == 4

testAmericanExpress = 
  isAmericanExpress 340000000000009 && isAmericanExpress 370000000000002 &&
  not (isAmericanExpress 340000001000009) && not (isAmericanExpress 370000000000003)

testMaster =
  isMaster 5500000000000004 && not (isMaster 5000000000000004)

testVisa =
  isVisa 4111111111111111 && not (isVisa 4111111111111112)

--todo confusion matrixx


--Exercise 8: 
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

accuses Peter x = (x == Matthew) || (x == Jack)

accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)

accuses Arnold x = (accuses Matthew x) /= (accuses Peter x)

accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers boy = [b | b <- boys, accuses b boy]

listPermutations :: [a] -> Integer -> [[a]]
listPermutations _ 0      = [[]]
listPermutations [] _     = []
listPermutations (x:xs) n = fmap (x:) (listPermutations xs (n - 1)) ++ listPermutations xs n

checkGuilty suspect liars = 
  all (==True) ([if b `elem` liars then not (accuses b suspect) else (accuses b suspect) | b <- boys])

getGuiltyAnswers :: [(Boy,[Boy])]
getGuiltyAnswers = let ans = [[(b,liars) | b <- boys, (checkGuilty b liars)]| liars <- (listPermutations boys 2)] in
  [ head a | a<-ans, length a /= 0] 

guilty, honest :: [Boy]
guilty = 
  let ans = getGuiltyAnswers in
  [ fst a | a <- ans]

honest =
  let ans = getGuiltyAnswers in
  let liars = [ snd a | a <- ans] in
  [ b | b <- boys, not (b `elem` (head liars))]
