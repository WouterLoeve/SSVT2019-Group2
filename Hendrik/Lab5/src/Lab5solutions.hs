module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Data.Function
import Test.QuickCheck
import Control.Monad
import SetOrd
import System.Random
import Lecture6 hiding (exM, composites, primeTestF, primeTestsF)
import Debug.Trace

-- Due to import structure and using the faster exM
primeTestF :: Integer -> IO Bool
primeTestF n = do 
   a <- randomRIO (2, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

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
 - We should check if the result is the same as the straightforward implementation.
 -  Q: How do we do this for very large values?
 -      A:
 - We should check if it is more efficient.
 -  Q: Can we time it? Should we use a benchmarking library (e.g. Criterion)?
 -      A:
 -  Q: Can we prove efficiency? Can we estimate the complexity in bits?
 -      A:
 - Can we check anything else?
 - WHEN is it faster?
-}
exM :: Integer -> Integer -> Integer -> Integer
exM x 0 m = 1 `rem` m
exM x 1 m = x `rem` m
exM x n m 
-- how to handle?
    | n < 0 = x ^ n
    | even n = exM x (n `div` 2) m ^ 2 `rem` m
    | otherwise = exM x (n - 1) m * exM x 1 m `rem` m

prop_isEqualExMExpM :: Integer -> Integer -> Integer -> Property
prop_isEqualExMExpM x n m = n >= 0 && m /= 0 ==> exM x n m == expM x n m

testExM :: IO ()
testExM = do
    print "Testing if the result of the computation is the same"
    verboseCheck prop_isEqualExMExpM

{- E2
 - Can we test nonprimality and infinity?
 -  Not really.
 - Can we prove nonprimality and infinity?
 -  Also not really?
-}
composites :: [Integer]
composites = filter (not . prime) [4..]

{- E3
 - In addition to the single trials, do some testing on the means of the smallest composite numbers accepted for any k.
 - We hypothesize that the least composite number scales exponentially with k due to needing to pass all tests.
 - Early testing results seem to agree with this.
 -
 - Note that the test may take a long time due to nondeterminism.
 - The running time of the test should scale somewhat exponentially too.
 - This is due to the combination of more or larger: k's used, repeated trials, 
 -  higher primes (in the composite generator), higher exponentiation.
 - Each aspect causes the others to increase as well.
 - Fairly conservative valeus were chosen, but it may still be necessary to restart the test in some cases.
-}
leastComposite :: Int -> IO Integer
leastComposite k = fix (\f (x:xs) -> do { b <- primeTestsF k x; if b then return x else f xs }) composites

meanI :: Integral a => [a] -> a
meanI xs = sum xs `div` fromIntegral (length xs)

avgComposites :: [Int] -> Int -> IO [Integer]
avgComposites ks n = mapM (\k -> meanI <$> replicateM n (leastComposite k)) ks

leastComposites :: IO ()
leastComposites = do
    leastComposite1 <- leastComposite 1
    leastComposite2 <- leastComposite 2
    leastComposite3 <- leastComposite 3
    print $ "Least composite for k = 1 is " ++ show leastComposite1
    print $ "Least composite for k = 2 is " ++ show leastComposite2
    print $ "Least composite for k = 3 is " ++ show leastComposite3
    print "Mean least composite for ks = [1, 2, 3, 5, 7] for trials n = 10"
    print =<< avgComposites [1, 2, 3, 5, 7] 10