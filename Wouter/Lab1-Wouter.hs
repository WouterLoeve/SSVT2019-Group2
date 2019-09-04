import Data.List
import Test.QuickCheck    
import Lab1


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


-- Exercise 5: Time: 15 min
