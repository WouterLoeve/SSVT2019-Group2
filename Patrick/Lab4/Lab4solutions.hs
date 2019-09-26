module Lab4solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
import Lecture3

{-
 - Exercise 1
 - Time: 40 min
-}

--generate arbitrary :: IO (Set Int)
--tmp_prop :: set char -> bool
--tmp_prop s = true

arbSet :: Arbitrary a => Int -> Gen (SetOrd.Set a)
arbSet s = do
    n <- choose (0, s)
    l <- vectorOf n arbitrary
    return $ Set l

instance Arbitrary a => Arbitrary (SetOrd.Set a) where
    arbitrary = sized arbSet

{-
 - Exercise 2
 - Time: 10 min
-}

setUnion, setIntersect, setDifference :: Ord a => Set a -> Set a -> Set a
setUnion      (Set xs) (Set ys) = Set (union xs ys)
setIntersect  (Set xs) (Set ys) = Set (intersect xs ys)
setDifference (Set xs) (Set ys) = Set ((xs \\ ys)++(ys \\ xs)) -- Sets are unordered, so both variations included

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
 - Time:  min
-}

-- Serial means that the domain of input is a subset of the range of the relation
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial l r = all (==True) [x `elem` relRange | x <- l]
    where relRange = (map fst r)


