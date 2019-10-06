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
 - Fairly conservative values were chosen, but it may still be necessary to restart the test in some cases.
-}
leastComposite :: Int -> [Integer] -> IO Integer
leastComposite k = fix (\f (x:xs) -> do b <- primeTestsF k x; if b then return x else f xs)

-- https://rosettacode.org/wiki/Averages/Mode#Haskell
mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)

modalComposites :: [Int] -> Int -> [Integer] -> IO [[Integer]]
modalComposites ks n l = mapM (\k -> mode <$> replicateM n (leastComposite k l)) ks

leastComposites :: IO ()
leastComposites = do
    leastComposite1 <- leastComposite 1 composites
    leastComposite2 <- leastComposite 2 composites
    leastComposite3 <- leastComposite 3 composites
    print $ "Least composite for k = 1 is " ++ show leastComposite1
    print $ "Least composite for k = 2 is " ++ show leastComposite2
    print $ "Least composite for k = 3 is " ++ show leastComposite3
    print "Modal least composites for ks = [1, 2, 3, 5, 7] for trials n = 10"
    print =<< modalComposites [1, 2, 3, 5, 7] 10 composites

{- E4
 - Fermat's little theorem states a^(p-1) mod p = 1 for any prime p and integer a not divisible by p.
 - Carmichael numbers are numbers for which this is true for any a coprime to them.
 - Thus a carmichael number will fool the Fermat primality test if only coprime a's are picked.
 - The generated list is a subset of the carmichael numbers.
 -
 - We expect the carmichael numbers to fool the Fermat primality test most of the time.
 - The proportion of this depends on the number of coprimes and amount of a's picked k.
 - The formula for this is ((ncoprimes n) / (n - 2))^k.
 - The base pass rate (k=1) of the first carmichael number 294409 is ~0.95%.
 - Thus we expect ~(0.95^k)% of least composites to be the first carmichael number.
 -  TODO: We can use this to test the Fermat primality test function. Use binomial test for that.
 - In fact, we expect the first number to be most common until around k=14, since 0.95^k > 0.5 for 0 < k < 14.
-}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

ncoprimes :: Integer -> Int
ncoprimes n = length $ filter (\m -> gcd n m == 1) [2..n-1]

{- The expected rate at which the nth carmichael number fools the Fermat test for k trials.
 - Note: please don't run this for any n > 0 (or maybe n = 1 if compiled).
 - I really just intend this to be used to check the proportion of first (zero-th) carmichaels passing the test.
-}
expectedPassRate :: Fractional a => Int -> Int -> a
expectedPassRate n k = (fromIntegral (ncoprimes m) / fromIntegral (m - 2)) ^ k
    where m = carmichael !! n

leastComposites' :: IO ()
leastComposites' = do
    leastComposite1 <- leastComposite 1 carmichael
    leastComposite2 <- leastComposite 2 carmichael
    leastComposite3 <- leastComposite 3 carmichael
    print $ "Least composite carmichael number for k = 1 is " ++ show leastComposite1
    print $ "Least composite carmichael number for k = 2 is " ++ show leastComposite2
    print $ "Least composite carmichael number for k = 3 is " ++ show leastComposite3
    print "Modal least composites for ks = [1, 2, 3, 5, 7] for trials n = 10"
    print =<< modalComposites [1, 2, 3, 5, 7] 10 carmichael
    -- I have not yet been able to run this part, dunno if there's an error?
    let leastComposite' k = fix (\f (x:xs) -> do b <- primeMR k x; if b then return x else f xs)
    leastComposite1' <- leastComposite' 1 carmichael
    leastComposite2' <- leastComposite' 2 carmichael
    leastComposite3' <- leastComposite' 3 carmichael
    print $ "Least composite carmichael number for MR test with k = 1 is " ++ show leastComposite1'
    print $ "Least composite carmichael number for MR test with k = 2 is " ++ show leastComposite2'
    print $ "Least composite carmichael number for MR test with k = 3 is " ++ show leastComposite3'
    

{- E5
 - Mersenne primes are primes of the form 2^n - 1.
 - The generated numbers are of this form by definition, so we only have to check primality.
 - TODO: do statistics.
 -  Q: how to deal with runtime? Can we only check primality statistics for the first n number?
-}
mrMPrimes :: Int -> [Integer] -> IO [Integer]
mrMPrimes 0 _ = return []
mrMPrimes n (p:ps) = do
    let mp = 2^p - 1
    b <- primeMR 1 mp
    if b then
        (mp :) <$> mrMPrimes (n - 1) ps
    else
        mrMPrimes n ps

someMRMPrimes :: IO ()
someMRMPrimes = do
    print "First 7 Mersenne primes obtained:"
    mprimes <- mrMPrimes 7 Lecture6.primes
    print mprimes
    print "Fake primes:"
    print $ filter (not . prime) mprimes

{- E6
 - TODO: proofs
 - Test coprimeness on both.
 - Q: can we test completeness?
 -  A: Just collect the trees and compare to the specification?
-}
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

{- E7
-}