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
 - Time:  min
-}

genRandomSet :: Int -> Int -> IO (Set Int)
genRandomSet amt cap = do
    x <- newStdGen
    return $ list2set (take amt . nub $ (randomRs (0,cap) x))

prop_unique :: Set Int -> Bool
prop_unique (Set a) = (length a) == (length $ nub a)

prop_ordered :: Set Int -> Bool
prop_ordered (Set a) = sort a == a

--genRandomSetTest = putStrLn (testRunHelper "")



-- testProperties = putStrLn (testRunHelper "testProperties" (length propertiesTestCases) (length (filter (==True) propertiesTestCases)))


{-
 - Exercise 2
 - Time:  min
-}

randomSets :: [IO (Set Int)]
randomSets = zipWith genRandomSet [10,20..1000] [10,20..1000]

intersection :: Set Int -> Set Int -> Set Int
intersection (Set a) (Set b) = list2set $ intersect a b

union' :: Set Int -> Set Int -> Set Int
union' (Set a) (Set b) = list2set $ union a b

diff :: Set Int -> Set Int -> Set Int
diff (Set a) (Set b) = list2set $ a \\ b

-- testIntersection = map intersection randomSets randomSets
-- testFuncs = do
--     testRunHelper


{-
 - Exercise 3
 - Time: 15 min
-}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = concat [[(a, b), (b, a)] | (a, b) <- xs]

{-
 - Exercise 4
 - Time:
-}

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial dom rel = all (==True) $ map (\x -> or [(elem) (x, y) rel | y <- dom]) dom

-- prop_emptySet :: Eq a => [a] -> Rel a -> Bool
-- prop_emptySet dom rel = isSerial [] []

{-
 - Exercise 5
 - Time: 25 min
-}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel = sort $ union ((union rel (rel @@ rel)) @@ rel) rel
