import Data.List
import Data.Char
import Test.QuickCheck    
import Lab1
import Control.Monad (replicateM)


-- Exercise 1: Time: 45 minutes if you include installing haskell and starting up :)

workshop1sum :: Int -> Int
workshop1sum n = sum([1..n])

workshop1hypo :: Int -> Int
workshop1hypo n = quot (n * (n+1)) 2

workshop1test n = workshop1hypo n == workshop1sum n

workshop2sum :: Int -> Int
workshop2sum n = sum([n^2 | n <- [1..n]])

workshop2hypo :: Int -> Int
workshop2hypo n = quot (n*(n+1)*(2*n+1)) 6

workshop2test n = workshop2sum n == workshop2hypo n

-- Exercise 2: Time: 15 minutes

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

-- Exercise 3: Time: 15 min
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where

insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fact :: Int -> Int
fact 0 = 1
fact n = foldr (*) 1 [1..n]

permutationLengthTest n = fact n == length (perms [1..n])

genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

{-
 * This property is also hard to test because of the same reason.
 * Computation time exceeds exponentially with increasing n.
 * When testing this function you are not only dependent on a correct implementation of the perm function.
 * But also on a correct implementation of your own function. 
 * Just like the previous exercise it is impossible to formally test the property 
   because there is an infinite amount of numbers that is to be tested. 
-}


-- Exercise 4: Time: 15 min
-- reversePrimes :: [Int]



reversePrimes = takeWhile (< 10000) (filter(\ n -> prime (reversal n)) primes)

{-
    Either find a library implementation which you are sure works ;) 
    Find the list that statisfies these condtions on the internet
    Or do it by hand
    Do not that all of these methods are very sub optimal. 
    You cant really be sure of that the library function works fully without proving it mathematically.
    It's possible that this list is not available online.
    Doing it by hand would logically be very tiresome.
-}


-- Exercise 5: Time: 20 min
sumOfPrimes = head (filter(\ n -> prime n) [sum (take 101 (drop n primes)) | n <- [1..]])

{-
 * Yes you have to test wether this answer is correct.
 * You cannot be sure that the function you implemented is correct.
 * A way to test this is to calculate it by hand but even that can have mistakes.
 * Another way could to compare you answer with your peers, of course this only works during this course.  
-}

-- Exercise 6: Time: 15 min
conjectureMul n = product (take n primes) + 1

testConjecture = take 10 (filter (\ n -> prime n == False) [conjectureMul n | n <- [1..]])

-- Exercise 7: Time: 45 min
luhn :: Integer -> Bool
luhn n = 
    let nums = reverse ([digitToInt x | x <- show n])
    in sum ([if odd i then luhnHelper (x*2) else x | (i, x) <- zip [0..] nums]) `mod` 10 == 0

luhnHelper :: Int -> Int
luhnHelper x | x > 9 = x - 9
             | x <= 9 = x

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = 
    let l = length [x | x <- show n]
        lValid = l == 15
        iin = read (take 2 (show n)) :: Integer
        iinValid = iin == 37 || iin == 34
    in iinValid && lValid

isMaster :: Integer -> Bool
isMaster n =
    let l = length [x | x <- show n]
        lValid = l == 16
        iinLength = [2, 4]
        iin = [read (take x (show n)) :: Integer | x <- iinLength ]
        iinValidFunc = filter (\ n -> (n >= 51 && n <= 55) || (n >= 2221 && n <= 2720))
        iinValid = length (iinValidFunc iin) >= 1
    in iinValid && lValid

isVisa :: Integer -> Bool
isVisa n = 
    let l = length [x | x <- show n]
        lValid = l == 16
        iin = read (take 1 (show n)) :: Integer
        iinValid = iin == 4
    in iinValid && lValid


-- Exercise 8: Time: 100 min

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


checkRequirements suspect liars = all (== True) [if elem x liars 
                                   then not (accuses x suspect) 
                                   else accuses x suspect 
                                   | x <- boys]

findCulprit = head (filter (\ (n, _, _) -> n == True) [(checkRequirements b x, b, x) | b <- boys, x <- listPermutations boys 2])

listPermutations :: [a] -> Integer -> [[a]]
listPermutations _ 0      = [[]]
listPermutations [] _     = []
listPermutations (x:xs) n = fmap (x:) (listPermutations xs (n - 1)) ++ listPermutations xs n

guilty :: [Boy]
guilty = [(\ (_, x, _) -> x) findCulprit]

honest :: [Boy]
honest = let
            xs = (\ (_, _, x) -> x) findCulprit
         in filter (\ x -> not (elem x xs)) boys