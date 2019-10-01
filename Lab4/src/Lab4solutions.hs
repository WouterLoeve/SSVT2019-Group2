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
listRandomSetLog :: IO [Set Int]
listRandomSetLog = mapM (randomSet . (10 ^)) [0 .. 8]

{-
 - Tests the properties with our random set generator.
-}
testSetOwnGen :: IO ()
testSetOwnGen = do
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
        xs = listRandomSetLog
        results = map f <$> xs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - QuickCheck tests
-}

{-
 - Generates an arbitrary set of length s.
-}
arbSet :: Arbitrary a => Int -> Gen (Set a)
arbSet s = do
    n <- choose (0, s)
    l <- vectorOf n arbitrary
    return $ Set l

{-
 - Makes a sized version of the aforementioned arbSet arbitrary 
    so no function has to be specified.
-}
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = sized arbSet

{-
 - Tests the properties with the quickhcheck set generator.
-}
testSetProp :: IO ()
testSetProp = do
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
 - Time: 60 min
 -}
setUnion, setIntersect, setDifference :: Ord a => Set a -> Set a -> Set a
setUnion      (Set xs) (Set ys) = list2set (xs ++ ys)
setIntersect  (Set xs) (Set ys) = list2set (xs `intersect` ys)
setDifference (Set xs) (Set ys) = list2set (xs \\ ys)

{-
 - Properties
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
 - Test functions for making pairs of the random generator types
 -}
testSetOperators = do
    print "Testing whether intersection is a subset of the union"
    quickCheck prop_unionInter
    print "Test union commutative property"
    quickCheck prop_setUnionCommutative
    print "Test union associative property"
    quickCheck prop_setUnionAssociative
    print "Test union distributive property"
    quickCheck prop_setUnionDistributive
    
    print "Test intersection commutative property"
    quickCheck prop_setIntersectCommutative
    print "Test intersection associative property"
    quickCheck prop_setIntersectAssociative
    print "Test intersection distributive property"
    quickCheck prop_setIntersectDistributive


{-
 - Tests a given property for two sets with random test 
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp2 :: (SetOrd.Set Int -> SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp2 f name = liftM2 (testRunHelper name) numCases numPass
    where 
        xs = listRandomSetLog
        ys = listRandomSetLog
        results = zipWith f <$> xs <*> ys
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

testSetOperatorsOwnGen :: IO ()
testSetOperatorsOwnGen = do
    ownGenTestProp2 prop_unionInter "Testing whether intersection is a subset of the union" >>= putStrLn

-- Fill with properties like in the quickcheck one.
    
{-
 - Exercise 3
 - Time: 20 min
-}
type Rel a = [(a,a)]

{-
 - A symmetric closure S of a relation R is defined as the union of R with it's converse relation.
 - The converse relation is defined as {(x,y) : (y,x) in R}. This means that we swap the two variables
 - in every tuple in R, so (1,2) becomes (2,1). The below implementation does exactly this:
 - 1. map and 'swap' are used to create the converse relation of R
 - 2. a union is made of R with the converse relation
 - 3. the result is sorted to make the result ordered
 -}
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
 - Because the symmetric closure of a relation R is always the union of R and it's conver relation,
 - we know the lower and upper size bound of the output of our symClos function.
 - 
 - The size of the output must always be greater or equal than the size of the input.
 - Why equal? consider the example of an empty relation `Rel []` and an already symmetric relation
 - such as `Rel [(1,2), (2,1)]`. Either example will not be changed by the function, and their output
 - will therefore be of the same length as the input.
 -
 - The size of the output must always be smaller than or equal than the size of the input times two.
 - Here, equal is included to handle cases such as `Rel [(1,2), (3,4)]` which will have output
 - `Rel [(1,2), (3,4), (2,1), (4,3)]` which is exactly twice the size 
 - (therefore satisfying the test 4 <= 2*2). The output length of symClos will be shorter if there are
 - duplicates.
 -}
prop_symClosLengthMin :: Rel Int -> Bool
prop_symClosLengthMin r = length (symClos r) >= length r

prop_symClosLengthMax :: Rel Int -> Bool
prop_symClosLengthMax r = length (symClos r) <= 2 * (length r)


testSymClos = do
    print "Testing symmetric closure length of R >= length of R"
    quickCheck prop_symClosLengthMin
    print "Testing symmetric closure length of R <= 2* length of R"
    quickCheck prop_symClosLengthMax

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
