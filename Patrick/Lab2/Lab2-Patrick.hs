import Lab2
import Test.QuickCheck
import Data.List
import Data.Char
import Data.Ord

-- Redefinition of forall, Lab2.hs is wrong
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Excercise 1: random in open interval
-- Usage: countQuartiles <number of samples
-- Returns an array with 4 items, for the 4 different quartiles
getQuartile :: [Float] -> Float -> Float -> Int
getQuartile list low high = length (filter (\x -> x>low && x<= high) list)
countQuartiles n = do
    probs n >>= \y -> return (
      (getQuartile y 0.0 0.25):
      (getQuartile y 0.25 0.5):
      (getQuartile y 0.5 0.75):
      (getQuartile y 0.75 1.0):[])

-- Excercise 2: recognizing Triangles, time spent: 1 hour
{-
findTriangle :: Integer -> Integer -> Integer -> Shape
findTriangle a b c =
    if (c >= (a+b)) then NoTriangle else
        if ((a==b) && (b==c)) then Equilateral else
            if (a^2 + b^2 == c^2) then Rectangular else
                if (a==b) || (a==c) || (b==c) then Isosceles else
                    Other
-}



findTriangle' :: Integer -> Integer -> Integer -> Shape
findTriangle' a b c | (c >= (a+b)) = NoTriangle
                    | ((a==b) && (b==c)) = Equilateral
                    | (a^2 + b^2 == c^2) = Rectangular
                    | ((a==b)||(a==c)||(b==c)) = Isosceles
findTriangle' _ _ _ = Other


-- Make param C the largest
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | (z >= x) && (z >= y) = findTriangle' x y z
               | (y >= x) && (y >= z) = findTriangle' x z y
               | (x >= y) && (x >= z) = findTriangle' y z x

triangle' :: [Integer] -> Shape
triangle' [x,y,z] = triangle x y z

testTriangleHelper l e =
   let p = permutations l in
      all (==True) [(triangle' i) == e | i <- p]
    
testTriangle=
   (testTriangleHelper [2,2,2] Equilateral) &&
   (testTriangleHelper [5,12,13] Rectangular) &&
   (testTriangleHelper [5,4,13] NoTriangle) && 
   (testTriangleHelper [5,5,8] Isosceles) &&
   (testTriangleHelper [3,4,6] Other) &&
   (testTriangleHelper [-2,2,2] NoTriangle)

{- In order to test this function, I decided to create a number of test cases with known
 - answers. The testTriangleHelper then feeds all permutations of a test case and checks
 - that every permutation results in the expected output
 - The effectiveness of this way of testing depends on the number of test cases and the
 - inclusion of corner cases. It is for example debatable whether a triangle with no height
 - (e.g. 5 5 10) is a real triangle.
 - The test function could be improved by providing better debug feedback, such as
 - which testcase failed (not just True/False output)
 -}

-- Excercise 3: Testing properties strength
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

propEven x = even x 
prop1 x = (even x && x > 3)
prop2 x = (even x || x > 3)
prop3And4 x = ((even x && x > 3) || even x)

{-STRONGEST
prop1
propEven
prop3And4
prop2
WEAKEST-}

-- Excercise 4: Recognizing Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = null (a \\ b) && null (b \\ a)
-- explanation: use list difference operator to find items only occuring in a or only in b.
-- If there are no items that are only in a or only in b then the list contain the same elements.

-- Properties
-- 1. length a & b must be the same
-- 2. The types must derive Eq
-- It is assumed that lists in Haskell can only contain a single type

testIsPermutation =
    (isPermutation [1,2,3] [2,1,3]) &&
    not (isPermutation [1,2,3] [2,4,3])
