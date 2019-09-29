module Lab4solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import System.Random
import SetOrd
import Lecture3

{-
 - Exercise 1
 - Time: 40 min
-}

--generate arbitrary :: IO (Set Int)
--tmp_prop :: set char -> bool
--tmp_prop s = true

--arbSetInt :: Int -> IO (Set Int)
--arbSetInt s = do
--    return sample $ resize 1000 (arbitrary :: IO (Set Int))

-- from scratch implementation
randIntSet s = do
    seed <- newStdGen
    return $ Set (take s (randoms seed :: [Int]))

-- QuickCheck implementation
arbSet :: Arbitrary a => Int -> Gen (Set a)
arbSet s = do
    n <- choose (0, s)
    l <- vectorOf n arbitrary
    return $ Set l

instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = sized arbSet

{-
 - Exercise 2
 - Time: 10 min
-}

setUnion, setIntersect, setDifference :: Ord a => Set a -> Set a -> Set a
setUnion      (Set xs) (Set ys) = Set (xs `union` ys)
setIntersect  (Set xs) (Set ys) = Set (xs `intersect` ys)
setDifference (Set xs) (Set ys) = Set (xs \\ ys)

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


testSetOperators = do
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
 - Exercise 3
 - Time: 10 min
-}
type Rel a = [(a,a)]

-- A symmetric closure S of R on set X is defined as the union of R with it's converse relation
symClos :: Ord a => Rel a -> Rel a
symClos r = union r [(b,a) | (a,b) <- r]

{-
 - Exercise 4
 - Time: 10 min
-}

-- Serial means that the domain of input is a subset of the range of the relation
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial l r = all (==True) [x `elem` relRange | x <- l]
    where relRange = (map fst r)

{-
 - Exercise 5
 - Time: 20 min
 - The trClos function recursively calculates the union of r and r @@ r until this
 - operation does not yield a different result than the input r anymore
-}
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos r = if r == r1 then r1 else trClos r1
    where r1 = union r (r @@ r)
