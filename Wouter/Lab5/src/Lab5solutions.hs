module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import Control.Conditional
import SetOrd
import System.Random
import Lecture5 hiding (exM, composites)
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
 - Exercise 1
 - Time: 120 min
-}
exM :: Integer -> Integer -> Integer -> Integer
exM g c n | n == 1 = 0
          | otherwise = exM' g c n 1

exM' g 0 n t = t
exM' g c n t = exM' g (c-1) n ans
    where ans = (g * t) `mod` n

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 g c n | n == 1 = 0
           | otherwise = foldl (\x y -> x * y `mod` n) 1 [g `mod` n | x <- [0..(c-1)]]
        
power :: Integer -> Integer -> Integer -> Integer
power a b c = a^b `mod` c

{-
 - Checks if the answer corresponds to the normal power function x^y `mod` n
-}
prop_checkPower :: [Integer] -> Bool
prop_checkPower [a, b, c] = exM a b c == power a b c

{-
 - Check whether the end result is actually smaller than the modulus
-}
prop_checkPowerMod :: [Integer] -> Bool
prop_checkPowerMod [a, b, c] = exM a b c < c

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

testExm :: IO ()
testExm = do
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPower
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPowerMod


{-
 - Exercise 2
 - Time: 120 min
 - A way of testing infinite lists is mentioned in the QuickCheck paper:
    https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
 - It says that two infinite lists are equal if all finite initial segments are equal.
 - This is not always the case, you can program in logic which makes the list stop working after a certain number
 - For example in a list comprehension, use an extra conditional to say that the number should be lower than some arbitrary number.
 - If that happens this way of testing doesn't work.
 - We can however, find some errors in the function looking at a initial finite part.
 - 

-}
composites :: [Integer]
composites = [x * y | x <- [2..], y <- [2..x]]

{-
 - Tests whether the number of factors for the first n composites is higher than 0.
 - A factor of 0 means that the number is a prime 
    since factors does not report the trivial case of 1 and itself as a factor and
    a number is prime if it is only divisble by itself and 1, which are both not reported.
 - Since the absence of these reports we can assume that the number is prime if it does not have factors.
-}
prop_compFactor :: Integer -> Bool
prop_compFactor n = all (not . null . factors)  (take (fromIntegral n) composites) 

testComposites :: IO ()
testComposites = do
    verboseCheck $ forAll genPositiveIntegers prop_compFactor

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


genPrime :: Gen Integer
genPrime = do
    index <- arbitrary `suchThat` (>10) 
    return $ primes !! index

-- prop_primetest :: IO 
-- prop_primetest = primeTestsF 40

-- testFermat :: IO ()
-- testFermat = do
--     verboseCheck $ forAll genPrime prop_primetest

{-
 - Found:
 - k = 1 -> 28, 33, 35  
 - k = 2 -> 55, 435
 - k = 3 -> 217, 561
 - k = 4 -> 703, 2465
 - k = 5 -> 561
 - k is the number of rounds used in the check. 
 - Increasing k will generally lead to the lower numbers 
    not fooling the test meaning that the minimum number 
    should go up when you increase k. 
 - This is not always the case since it remains a probabalistic function.
-}

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
 - Time: 200 min
 - Source: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
-}
sameBitLength :: Integer -> Integer -> Bool
sameBitLength a b = ceiling (logBase 2 (fromIntegral a)) == ceiling (logBase 2 (fromIntegral b))

genRandomInt :: IO Integer
genRandomInt = do
    r <- randomRIO (2^256, 2^256-1)
    return $ abs r

rsaPrime :: IO (Integer, Integer)
rsaPrime = do
        p <- getRsaPrime
        q <- getRsaPrime
        if sameBitLength p q then
            return (p, q)
        else
            rsaPrime

-- PrimeMR function is really slow for large primes, even on low k's. How do we fix?
getRsaPrime :: IO Integer
getRsaPrime = do
    p <- genRandomInt
    if even p then
        getRsaPrime
    else do
        pPrime <- primeMR 40 p
        if pPrime then
            return p
        else
            getRsaPrime

genRSAencrypt :: Integer -> IO Integer
genRSAencrypt tot = do
    e <- randomRIO (1, tot)
    if gcd e tot == 1 then
        return e
    else
        genRSAencrypt tot

-- IIRC b is the smallest so its the value of d.
genRSAdecrypt :: Integer -> Integer -> IO Integer
genRSAdecrypt e tot = do
    d <- randomRIO (1, tot)
    let (_, b) = fctGcd (e) tot
    return b

genRSAKeys :: IO (Integer, Integer, Integer)
genRSAKeys = do
    (p, q) <- rsaPrime
    let n = p * q
    let tot = lcm (p-1) (q-1)
    e <- genRSAencrypt tot
    d <- genRSAdecrypt e tot

    return (e, d, n)
    
rsaEncodeM :: IO (Integer,Integer) -> Integer -> IO Integer
rsaEncodeM keys m = do
    (e, n) <- keys
    return $ rsaEncode (e, n) m

rsaDecodeM :: IO (Integer,Integer) -> Integer -> IO Integer
rsaDecodeM keys m = do
    (d, n) <- keys
    return $ rsaDecode (d, n) m