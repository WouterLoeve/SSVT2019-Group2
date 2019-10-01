module Lab4solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
import Lecture3

import System.Random
import Data.Function

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
 - Time: min
-}
genSetInt :: Int -> (Int, Int) -> IO (Set Int)
genSetInt lengthR numR = do
    n <- getStdRandom (randomR (0, lengthR))
    gen <- getStdGen
    return . list2set . take n $ randomRs numR gen

genSetInt' :: Int -> (Int, Int) -> Gen (Set Int)
genSetInt' lengthR numR = Set <$> resize lengthR (listOf arbitrary)

{- E2
-}
set2list :: Set a -> [a]
set2list (Set l) = l

(>*<) :: Ord a => Set a -> Set a -> Set a
a >*< b = list2set $ set2list a `intersect` set2list b

(>+<) :: Ord a => Set a -> Set a -> Set a
a >+< b = list2set $ set2list a ++ set2list b

(>\\<) :: Ord a => Set a -> Set a -> Set a
a >\\< b = list2set $ set2list a \\ set2list b

{- E3
-}
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = r `union` swap <$> r 

{- E4
-}
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial d r = all (`elem` r') d
    where r' = fst <$> filter (\ (_, b) -> b `elem` d) r

{- E5
-}
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos r = fix (\ f s -> if s == s `union` (r @@ s) then s else f $ s `union` (r @@ s)) r

{- E7
 - The basic relation R = [(a, b)] is a counterexample; 
 -  the transitive closure of the symmetric closure will include the reflexive closure as well.
 - i.e. sc tc [(a, b)] = [(a, b), (b, a)], tc sc [(a, b)] = [(a, b), (b, a), (a, a), (b, b)]
-}
