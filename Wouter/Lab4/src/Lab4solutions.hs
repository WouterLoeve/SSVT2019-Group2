module Lab4solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
import System.Random
import Lecture3

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
 - Time: 10 min
-}
randomSet :: Int -> IO (SetOrd.Set Int)
randomSet x = do
    g <- getStdGen
    return $ list2set $ take x (randoms g :: [Int])

intSet :: Int -> Gen (SetOrd.Set Int)
intSet n = list2set <$> vectorOf n arbitrary

intSetSized :: Gen (SetOrd.Set Int)
intSetSized = sized $ \n -> resize (round (sqrt n)) arbitrary

{-
 - Exercise 2
 - Time: 10 min
-}
set2list (Set l) = l

unionSet' :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
unionSet' xs ys = list2set $ union (set2list xs) (set2list ys) 

intersectionSet :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
intersectionSet xs ys = list2set $ intersect (set2list xs) (set2list ys) 

differenceSet :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
differenceSet xs ys = list2set $ (set2list xs) \\ (set2list ys)

prop_unionInter :: Ord a => SetOrd.Set a -> SetOrd.Set a -> Bool
prop_unionInter xs ys =  (intersectionSet xs ys) `subSet` (unionSet' xs ys)

testSetOperators = do
    verboseCheck $ forAll intSetSized prop_unionInter 
--         arbForm' :: Integral a => a -> Gen Form
-- arbForm' 0 = fmap Prop (suchThat arbitrary (>0))
-- arbForm' n = frequency
--     [ (1, fmap      Prop (suchThat arbitrary (>0))),
--       (1, fmap      Neg param),
--       (1, liftM2    Impl param param),
--       (1, liftM2    Equiv param param),
--       (1, fmap      Dsj (vectorOf 2 param)),
--       (1, fmap      Cnj (vectorOf 2 param)) ]
--     where param = arbForm' (n `div` 2)

-- instance Arbitrary Form where
--     arbitrary = sized $ \ n -> arbForm' (round (sqrt (fromIntegral n)))