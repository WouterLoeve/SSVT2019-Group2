module Lab4solutions where
import Data.List
import Data.Char
import Data.Function
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
 - Time: 30 min
-}

{-
 - Testing from scratch
-}

{-
 - Generates a random set with x values between 0 and x.
-}
randomSet :: Int -> IO (SetOrd.Set Int)
randomSet x = do
    g <- newStdGen
    return $ list2set $ take x (randomRs (0, x) g :: [Int])

{-
 - Generates random sets with lengths 1 to 10^8.
-}
listRandomSetLin :: IO [Set Int]
listRandomSetLin = mapM randomSet [0 .. 10^2-1]

{-
 - Tests the properties with our random set generator.
-}
testSetOwnGen :: IO ()
testSetOwnGen = do
    print "------ Testing Set Operators Own Generator ------"
    ownGenTestProp prop_setNoDups "Checking whether no duplicates are present in the set" >>= putStrLn
    ownGenTestProp prop_setOrdered "Checking whether the set is ordered" >>= putStrLn
    

{-
 - Tests a given property for a sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp :: (SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLin
        results = map f <$> xs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - QuickCheck tests
-}

{-
 - Generates an arbitrary set of length s.
-}
arbSet :: (Ord a, Arbitrary a) => Int -> Gen (Set a)
arbSet s = do
    n <- choose (0, s)
    l <- vectorOf n arbitrary
    return $ list2set l

{-
 - Makes a sized version of the aforementioned arbSet arbitrary 
    so no function has to be specified.
-}
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized arbSet

{-
 - Tests the properties with the quickhcheck set generator.
-}
testSetProp :: IO ()
testSetProp = do
    print "------ Testing Set QuickCheck ------"
    print "Testing whether Sets contain duplicates"
    quickCheck prop_setNoDups 

    print "Testing whether Sets are ordered"
    quickCheck prop_setOrdered 

{-
 - Properties tests
-}

{-
 - A set should not contain duplicates.
-}
prop_setNoDups :: SetOrd.Set Int -> Bool
prop_setNoDups (Set a) = length a == length (nub a)

{-
 - A set is ordered.
-}
prop_setOrdered :: Set Int -> Bool
prop_setOrdered (Set a) = sort a == a

{-
 - Exercise 2
 - We devised a bunch of properties based on the mathematical set properties and 
    used both the quickcheck generator as our own generator to test these.
 - We found that these properties cover a lot of behaviour even implementing combinations of the three functions,
 -}
setUnion, setIntersect, setDifference :: Ord a => Set a -> Set a -> Set a
{-
 - Takes the union of two sets. Implemented with append since the set logic takes out all duplicates
 -}
setUnion      (Set xs) (Set ys) = list2set (xs ++ ys)

{-
 - Takes the intersectino of two sets.
 -}
setIntersect  (Set xs) (Set ys) = list2set (xs `intersect` ys)

{-
 - Takes the relative complement of two sets.
 -}
setDifference (Set xs) (Set ys) = list2set (xs \\ ys)

{-
 - Properties
 -}

 {-
 - (A n B) subSet (A u B)
 -}
prop_unionInter :: Set Int -> Set Int -> Bool
prop_unionInter xs ys = (xs `setIntersect` ys) `subSet` (xs `setUnion` ys)

-- Helper function to compare sets (because order might differ)
setEqual :: Eq a => Set a -> Set a -> Bool
setEqual (Set a) (Set b) = null (a \\ b) && null (b \\ a)

-- A u B == B u A
prop_setUnionCommutative :: Set Int -> Set Int -> Bool
prop_setUnionCommutative a b = setEqual (setUnion a b) (setUnion b a)

-- (A u B) u C == A u (B u C)
prop_setUnionAssociative :: Set Int -> Set Int -> Set Int -> Bool
prop_setUnionAssociative a b c = setUnion (setUnion a b) c == setUnion a (setUnion b c)

-- A u (B n C) == (A u B) n (A u C)
prop_setUnionDistributive :: Set Int -> Set Int -> Set Int -> Bool 
prop_setUnionDistributive a b c = setUnion a (setIntersect b c) == setIntersect (setUnion a b) (setUnion a c)

-- A n B == B n A
prop_setIntersectCommutative :: Set Int -> Set Int -> Bool
prop_setIntersectCommutative a b = setEqual (setIntersect a b) (setIntersect b a)

-- (A n B) n C == A n (B n C)
prop_setIntersectAssociative :: Set Int -> Set Int -> Set Int -> Bool
prop_setIntersectAssociative a b c = setIntersect (setIntersect a b) c == setIntersect a (setIntersect b c)

-- A n (B u C) == (A n B) u (A n C)
prop_setIntersectDistributive :: Set Int -> Set Int -> Set Int -> Bool
prop_setIntersectDistributive a b c = setIntersect a (setUnion b c) == setUnion (setIntersect a b) (setIntersect a c)

{-
 - (A \ B) u B == A u B
 -}
prop_setUnionDifference :: Set Int -> Set Int -> Bool
prop_setUnionDifference a b = (a `setDifference` b) `setUnion` b == a `setUnion` b

{-
 - (A \ B) subSet A
 -}
prop_setUnionDifference2 :: Set Int -> Set Int -> Bool
prop_setUnionDifference2 a b = (a `setDifference` b) `subSet` a

{-
 - C \ (A n B) == (C \ A) u (C \ B)
 -}
prop_setDifferenceIntersectUnion :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceIntersectUnion a b c = (c `setDifference` (a `setIntersect` b)) == (c `setDifference` a) `setUnion` (c `setDifference` b) 

{-
 - C \ (A u B) == (C \ A) n (C \ B)
 -}
prop_setDifferenceUnionIntersect :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceUnionIntersect a b c = c `setDifference` (a `setUnion` b) == (c `setDifference` a) `setIntersect` (c `setDifference` b)

prop_setDifferenceDist :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceDist a b c = c `setDifference` (b `setDifference` a) == (c `setIntersect` a) `setUnion` (c `setDifference` b)

{-
 - (B \ A) n C == (B n C) \ A == B n (C \ A)
 -}
prop_setDifferenceDiff :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceDiff a b c = (b `setDifference` a) `setIntersect` c == (b `setIntersect` c) `setDifference` a

{-
 - (B \ A) u C == (B u C) \ (A \ C)
 -}
prop_setDifferenceUnion :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceUnion a b c = (b `setDifference` a) `setUnion` c == (b `setUnion` c) `setDifference` (a `setDifference` c)

{-
 - A \ A = 0
 - 0 = Empty set
 -}
prop_setDifferenceIdentity :: Set Int -> Bool
prop_setDifferenceIdentity a = (a `setDifference` a) == list2set []
{-
 - 0 \ A = 0
 -}
prop_setDifferenceEmpty :: Set Int -> Bool
prop_setDifferenceEmpty a = (list2set []) `setDifference` a == list2set []

{-
 - A \ 0 = A
 -}
prop_setDifferenceEmpty2 :: Set Int -> Bool
prop_setDifferenceEmpty2 a = a `setDifference` (list2set []) == a

{-
 - Quickcheck test caller using generator from exercise 1.
 -}
testSetOperators = do
    print "------ Testing Set Operators QuickCheck ------"
    print "Testing whether intersection is a subset of the union"
    quickCheck prop_unionInter
    print "Testing union commutative property"
    quickCheck prop_setUnionCommutative
    print "Testing union associative property"
    quickCheck prop_setUnionAssociative
    print "Testing union distributive property"
    quickCheck prop_setUnionDistributive

    print "Testing intersection commutative property"
    quickCheck prop_setIntersectCommutative
    print "Testing intersection associative property"
    quickCheck prop_setIntersectAssociative
    print "Testing intersection distributive property"
    quickCheck prop_setIntersectDistributive
    print "Testinging whether difference of two sets unioned with the removed parts equals the union."
    quickCheck prop_setUnionDifference
    print "Testinging whether the difference between a and b is a subset of the original"
    quickCheck prop_setUnionDifference2
    
    print "Testing Difference distributative intersection property "
    quickCheck prop_setDifferenceIntersectUnion
    print "Testing Difference distributative union property"
    quickCheck prop_setDifferenceUnionIntersect
    print "Testing Difference distributative property"
    quickCheck prop_setDifferenceDist
    print "Testing Difference commutative intersect property"
    quickCheck prop_setDifferenceDiff
    print "Testing Difference commutative union property"
    quickCheck prop_setDifferenceUnion
    print "Testing Difference identity"
    quickCheck prop_setDifferenceIdentity
    print "Testing Difference Empty and A"
    quickCheck prop_setDifferenceEmpty
    print "Testing Difference A and empty"
    quickCheck prop_setDifferenceEmpty2


{-
 - Tests a given property for two sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp2 :: (SetOrd.Set Int -> SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp2 f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLin
        ys = listRandomSetLin
        results = zipWith f <$> xs <*> ys
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - Tests a given property for three sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp3 :: (SetOrd.Set Int -> SetOrd.Set Int -> SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp3 f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLin
        ys = listRandomSetLin
        zs = listRandomSetLin
        results = zipWith3 f <$> xs <*> ys <*> zs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - Caller for the own generator properties
-}
testSetOperatorsOwnGen :: IO ()
testSetOperatorsOwnGen = do
    print "------ Testing Set Operators Own Generator ------"
    ownGenTestProp2 prop_unionInter "Testing whether intersection is a subset of the union" >>= putStrLn
    ownGenTestProp2 prop_setUnionCommutative "Testing union commutative property" >>= putStrLn
    ownGenTestProp3 prop_setUnionAssociative "Testing union associative property" >>= putStrLn
    ownGenTestProp3 prop_setUnionDistributive "Testing union distributive property" >>= putStrLn
    ownGenTestProp2 prop_setUnionDifference "Testing whether difference of two sets unioned with the removed parts equals the union." >>= putStrLn
    ownGenTestProp2 prop_setUnionDifference2 "Testing whether the difference between a and b is a subset of the original" >>= putStrLn
     
    ownGenTestProp2 prop_setIntersectCommutative "Testing intersection commutative property" >>= putStrLn
    ownGenTestProp3 prop_setIntersectAssociative "Testing intersection associative property" >>= putStrLn
    ownGenTestProp3 prop_setIntersectDistributive "Testing intersection distributive property" >>= putStrLn
     
    ownGenTestProp3 prop_setDifferenceIntersectUnion "Testing Difference distributative intersection property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceUnionIntersect "Testing Difference distributative union property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceDist "Testing Difference distributative property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceDiff "Testing Difference commutative intersect property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceUnion "Testing Difference commutative union property" >>= putStrLn
    ownGenTestProp prop_setDifferenceIdentity "Testing Difference identity" >>= putStrLn
    ownGenTestProp prop_setDifferenceEmpty "Testing Difference Empty and A" >>= putStrLn
    ownGenTestProp prop_setDifferenceEmpty2 "Testing Difference A and empty" >>= putStrLn
    
{-
 - Exercise 3
 - Time: 20 min
-}
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` (swap <$> r) 

{-
 - Exercise 4
 - Time: 20 min
 - Properties
    (Precondition: reflexive) -> Serial (construction hint: transitive of symmetric is reflexive)

-}
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial dom rel = all (\x -> or [(x, y) `elem` rel | y <- dom]) dom


{-
 - Exercise 5
 - Time: 30 min
-}

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos r = sort $ fix (\ f s -> if s == s `union` (r @@ s) then s else f $ s `union` (r @@ s)) r

{-
 - Exercise 6
 - Time: 30 min
-}


{-
 - Exercise 7
 - Time: 30 min
 - r = [(a,b)] where b =/= a
-}
illustrateDiff = do
    print $ "Symmetric closure of the transitive closure " ++ show (symClos $ trClos [(1,2)])
    print $ "Transitive closure of the symmetric closure " ++ show (trClos $ symClos [(1,2)])

{-
 - Exercise 9
 - Time:  min
 -}
