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
composites = [x * y | x <- [2..], y <- [2..x]]

{-
 - Exercise 3
 - Time: 120 min
-}

test_primeTestsF :: Int -> [Integer] -> IO String
test_primeTestsF k gen = show <$> test_primeTestF' k gen

show_primeTests3 = do
    print "K = 1"
    test_primeTestsF 1 composites >>= putStrLn
    print "K = 2"
    test_primeTestsF 2 composites >>= putStrLn
    print "K = 3"
    test_primeTestsF 3 composites >>= putStrLn
    print "K = 4"
    test_primeTestsF 4 composites >>= putStrLn
    print "K = 5"
    test_primeTestsF 5 composites >>= putStrLn

{-
 - Found:
 - k = 1 -> 28, 33, 35  
 - k = 2 -> 55, 435
 - k = 3 -> 217, 561
 - k = 4 -> 703, 2465
 - k = 5 -> 561

-}

-- test_primeTestF' :: Int -> [Integer] -> [Integer] -> [Integer]
test_primeTestF' :: Int -> [Integer] -> IO Integer
test_primeTestF' k [] = return (-1)
test_primeTestF' k (x:xs) = do
                a <- primeTestsF k x
                if a then
                    return x
                else
                    test_primeTestF' k xs


                    
{-
 - Exercise 4
 - Time: 10 min
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
        k <- [2..], 
        prime (6*k+1), 
        prime (12*k+1), 
        prime (18*k+1) ]

-- test_primeTestsF k = test_primeTestF' k (carmichael []

{-
 - Found:
 - k = 1 -> 294409
 - k = 2 -> 294409
 - k = 3 -> 294409
 -}

show_primeTests4 = do
    test_primeTestsF 1 carmichael >>= putStrLn
    test_primeTestsF 2 carmichael >>= putStrLn
    test_primeTestsF 3 carmichael >>= putStrLn

{-
 - Exercise 5
 - Time: 20 min
-}
getMersennePrime :: Int -> Integer -> (Bool, Integer)
getMersennePrime x k = (mrComposite (2^p-1) k, p)
    where p = primes !! x

{-
 - Exercise6
 - Time: 30 min
-}
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

{-
 - You can check that those pairs are in the tree by writing the following quickCheck property.
 - You can use the collect version used in the workshop to produce the pairs of the tree.
 - These can then be used to check against a list of genereted co-primes till n.
 - Proof?
-}

prop_CoPrimeTree :: (Integer -> Tree (Integer, Integer)) -> Integer -> Bool
prop_CoPrimeTree tree n = sort (collect' (tree n)) == sort ([(x, y) | x <- [1..n], y <- [1..n], gcd x y == 1])

collect' :: Tree a -> [a]
collect' a = (foldT (\ x xs -> x : concat xs) a) 

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

testTrees :: IO ()
testTrees = do
    quickCheck $ forAll genPositiveIntegers (prop_CoPrimeTree tree1)
    quickCheck $ forAll genPositiveIntegers (prop_CoPrimeTree tree2)

{-
 - Exercise 7
 - Time: 30 min
-}
sameBitLength a b = (ceiling $ logBase 2 a) == (ceiling $ logBase 2 b)

-- rsaPrime = 

genRandomInt :: IO Integer
genRandomInt = do
    g <- newStdGen
    return $ (random g :: Integer)

-- rsaPrime' = head $ filterM (\ x y -> mrComposite x 100 && mrComposite y 100)
--             [(p, q) | p <- genRandomInt, q <- genRandomInt]
rsaPrime' ::  IO ([Integer], [Integer])
rsaPrime' = do
        p <- genRandomInt
        q <- genRandomInt
        Control.Monad.when (mrComposite p 100 && mrComposite q 100)
            return (p, q)
        rsaPrime'
        