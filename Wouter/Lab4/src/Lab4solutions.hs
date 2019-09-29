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

set2list (Set l) = l
{-
 - Exercise 1
 - Time: 30 min
-}

{-
 - Testing from scratch
-}
randomSet :: Int -> IO (SetOrd.Set Int)
randomSet x = do
    g <- newStdGen
    return $ list2set $ take x (randomRs (0, x) g :: [Int])

listRandomSetLog :: IO [Set Int]
listRandomSetLog = mapM randomSet [0,10..100]

testSetOwnGen :: IO ()
testSetOwnGen = do
    ownGenTestProp prop_setNoDups "Checking whether no duplicates are present in the set" >>= putStrLn
    ownGenTestProp prop_setOrdered "Checking whether the set is ordered" >>= putStrLn
    ownGenTestProp prop_setOrdered "Checking whether the set is ordered" >>= putStrLn
    

{-
 - Tests a given property for a sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp :: (SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLog
        results = map f <$> xs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - QuickCheck tests
-}
intSet :: Int -> Gen (SetOrd.Set Int)
intSet n = list2set <$> vectorOf n arbitrary

intSetSized :: Gen (SetOrd.Set Int)
intSetSized = sized $ \n -> intSet (fromIntegral n)

testSetProp :: IO ()
testSetProp = do
    print "Testing whether Sets contain duplicates"
    quickCheck $ forAll intSetSized prop_setNoDups 

    print "Testing whether Sets are ordered"
    quickCheck $ forAll intSetSized prop_setOrdered 


{-
 - Properties tests
-}

prop_setNoDups :: SetOrd.Set Int -> Bool
prop_setNoDups xs = length (set2list xs) == length (nub $ set2list xs)

prop_setOrdered :: Ord a => Set a -> Bool
prop_setOrdered xs = sort (set2list xs) == set2list xs 

{-
 - Exercise 2
 - Time: 60 min
-}
unionSet' :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
unionSet' xs ys = list2set $ union (set2list xs) (set2list ys) 

intersectionSet :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
intersectionSet xs ys = list2set $ intersect (set2list xs) (set2list ys) 

differenceSet :: Ord a => SetOrd.Set a -> SetOrd.Set a -> SetOrd.Set a
differenceSet xs ys = list2set $ set2list xs \\ set2list ys

{-
 - Properties
 -}

prop_unionInter :: Ord a => (SetOrd.Set a, SetOrd.Set a) -> Bool
prop_unionInter (xs, ys) = intersectionSet xs ys `subSet` unionSet' xs ys


{-
 - Test functions for making pairs of the random generator types
 -}
testSetOperators :: IO ()
testSetOperators = do
    print "Testing whether intersection is a subset of the union"
    quickCheck $ forAll twoIntSetSized prop_unionInter 


twoIntSetSized :: Gen (Set Int, Set Int)
twoIntSetSized = (,) <$> intSetSized <*> intSetSized


{-
 - Tests a given property for two sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp2 :: ((SetOrd.Set Int, SetOrd.Set Int) -> Bool) -> String -> IO String
ownGenTestProp2 f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLog
        ys = listRandomSetLog
        results = map f <$> (zip <$> xs <*> ys)
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

testSetOperatorsOwnGen :: IO ()
testSetOperatorsOwnGen = do
    ownGenTestProp2 prop_unionInter "Testing whether intersection is a subset of the union" >>= putStrLn

    {-
 - Exercise 3
 - Time: 20 min
-}
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = symClos' xs []

symClos' [] ys = ys
symClos' (x:xs) ys = symClos' xs (ys ++ [x, (b,a)])
    where (a, b) = x

{-
 - Exercise 4
 - Time: 20 min
-}
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rels = all (==True) [isSerial' x rels domain | x <- domain]

isSerial' x xs domain = True `elem` [(x,y) `elem` xs | y <- domain]

{-
 - Exercise 5
 - Time: 30 min
-}

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a 
-- trClos rels = rels ++ concat [ concat $ trClos' r rels | r <- rels]

-- trClos' (x,y) rels = [ (x,z) : concat (trClos' (x,z) rels) | (y', z) <- rels, y' == y, (x,y) /= (y',z), x /= z]

-- trClos' (x,y) rels |  =

-- SORT?



trClos :: Ord a => Rel a -> Rel a 
trClos rels = sort $ trClos' rels (length rels)

trClos' rels l | len > l = trClos' new len
               | otherwise = rels
    where 
        len = length new
        new = nub $ (rels @@ rels) ++ rels

{-
 - Exercise 6
 - Time: 30 min
-}


{-
 - Exercise 7
 - Time: 30 min
-}
illustrateDiff = do
    print $ "Symmetric closure of the transitive closure " ++ show (symClos $ trClos [(1,2),(2,3),(3,4)])
    print $ "Transitive clouse of the symmetric closure " ++ show (trClos $ symClos [(1,2),(2,3),(3,4)])
