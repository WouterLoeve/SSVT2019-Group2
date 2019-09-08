import Lab1
import Data.List
import Test.QuickCheck
import Data.Char

genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

{- 
 * Exercise 1 
 * Commands: quickCheck $ forAll genPositiveIntegers testSumSquares
 * 
-}

sumSquares' :: Int -> Int
sumSquares' n = sum [ k^2 | k <- [1..n] ]

sumSquares :: Int -> Int
sumSquares n = quot (n*(n + 1)*(2*n + 1)) 6

testSumSquares :: Int -> Bool
testSumSquares n = sumSquares' n == sumSquares n

{- 
 * Part 2
 * Commands: quickCheck $ forAll genPositiveIntegers testSumCubes
 * 
-}

sumCubes' :: Int -> Int
sumCubes' n = sum [ k^3 | k <- [1..n] ]

sumCubes :: Int-> Int
sumCubes n = (^) (quot (n*(n + 1)) 2) 2

testSumCubes :: Int -> Bool
testSumCubes n = sumCubes' n == sumCubes n


{- 
 * Exercise 2
-}

testPowerCardinality :: Int -> Bool
testPowerCardinality n = length (subsequences [1..n]) == 2^n

{- 
 * This property is hard to test because for large number the function runs for a long time. 
 * There is also an infinite number of natural numbers to test this with. 
 * This makes it impossible to test the function fully with random tests. 
 * At this point you should also ask yourself wether you are testing the library functions or your own code. 
 * So to conclude what you are testing is wether the function gives correct output on the things you put into it. 
 * The result of this test is should be the conclusion of that your code works practically, not formally.
-}

{- 
 * Exercise 3
-}

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fac :: Int -> Int
fac 0 = 1
fac n = product [1..n]

testPermsCardinality :: Int -> Bool
testPermsCardinality n = length (perms [1..n]) == fac n

{-
 * This property is also hard to test because of the same reason.
 * Computation time exceeds exponentially with increasing n.
 * When testing this function you are not only dependent on a correct implementation of the perm function.
 * But also on a correct implementation of your own function. 
 * Just like the previous exercise it is impossible to formally test the property 
 *  because there is an infinite amount of numbers that is to be tested. 
-}

{- 
 * Exercise 4
-}
reversiblePrimes :: [Integer]
reversiblePrimes = takeWhile (< 10000) (filter (prime . reversal) primes)

{-
 * Either find a library implementation which you are sure works ;) 
 * Find the list that statisfies these condtions on the internet
 * Or do it by hand
 * Do note that all of these methods are very sub optimal. 
 * You cant really be sure of that the library function works fully without proving it mathematically.
 * It's possible that this list is not available online.
 * Doing it by hand would logically be very tiresome.
-}

{- 
 * Exercise 5
-}

sum101Primes :: Int -> Integer
sum101Primes n = sum (take 101 (drop n primes))

smallestConsecutivePrime :: Integer
smallestConsecutivePrime = head (filter prime (map sum101Primes [1..]))

{-
 * Answer: 37447
 * There doesn't seem to be a way to test this as the implementation
 *  of the test can at best be as accurate as the original
 *  (i.e. a different implementation or by hand).
-}

{- 
 * Exercise 6
-}
productPlusOneNotPrime :: [Integer] -> Bool
productPlusOneNotPrime l = not (prime (product l + 1))

smallestCounterExample :: [Integer]
smallestCounterExample = head (filter productPlusOneNotPrime (map (`take` primes) [1..]))

{- 
 * 30031 is the smallest counterexample, this is the list [2,3,5,7,11,13]
-}


{- 
 * Exercise 7
-}
digits :: Integer -> [Integer]
digits = map (read . return) . show

luhndouble :: [Integer] -> [Integer]
luhndouble l = [if odd (snd x) then fst x else sum (digits (fst x * 2)) | x <- zip (reverse l) [1..]]

luhn :: Integer -> Bool
luhn n = sum (luhndouble (digits n)) `mod` 10 == 0

checkIIN :: [Integer] -> [Integer] -> Bool
checkIIN prefixes list = any (`isPrefixOf` list) (map digits prefixes)

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

{- 
 * Test design
-}


{- 
 * Exercise 8
-}
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True 

accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)

accuses Arnold x = accuses Matthew x /= accuses Peter x 

accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers boy = [b | b <- boys, accuses b boy]

checkRequirements :: Boy -> [Boy] -> Bool
checkRequirements suspect liars = all (== True) [if x `elem` liars 
                                   then not (accuses x suspect) 
                                   else accuses x suspect 
                                   | x <- boys]

findCulprit :: (Bool, Boy, [Boy])
findCulprit = head (filter (\ (n, _, _) -> n) [(checkRequirements b x, b, x) | b <- boys, x <- listPermutations boys 2])

listPermutations :: [a] -> Integer -> [[a]]
listPermutations _ 0      = [[]]
listPermutations [] _     = []
listPermutations (x:xs) n = fmap (x:) (listPermutations xs (n - 1)) ++ listPermutations xs n

guilty :: [Boy]
guilty = [(\ (_, x, _) -> x) findCulprit]

honest :: [Boy]
honest = let
            xs = (\ (_, _, x) -> x) findCulprit
         in filter (`notElem` xs) boys