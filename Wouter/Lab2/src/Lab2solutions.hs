module Lab2solutions where
import Data.List
import Data.Char
import Lab2

{- 
 - Exercise 1
 - Time: 30 min
 - A: In each quartile there are about 2500 items. Variance report?
-}


quartile :: RealFrac a => [a] -> [Int]
quartile xs = [length $ filter (\ n -> n == y) [ ceiling (x * 4) | x <- xs ] | y <- [1..4]]

quartileTest :: IO [Int]
quartileTest = fmap (\ y -> quartile y) (probs 10000)

{- 
 - Exercise 2
 - Time: 60 mins
-}
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | checkTriangle a b c == NoTriangle = NoTriangle
               | length checkFilter == 0 = Other
               | otherwise = head checkFilter
    where 
        (a, b, c) = orderTriangle x y z
        equil = checkEquilateral a b c
        rect = checkRectangular a b c
        isos = checkIsosceles a b c
        checkFilter = filter (/=Other) [equil, rect, isos]
    

orderTriangle :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
orderTriangle x y z | (z > y) && (z > x) = (x, y, z)
                    | (x > y) && (x > z) = (y, z, x)
                    | (y > x) && (y > z) = (x, z, y)
                    | otherwise = (x, y, z)

checkEquilateral :: Integer -> Integer -> Integer -> Shape
checkEquilateral a b c | (a == b && b == c) = Equilateral
                       | otherwise = Other

checkRectangular :: Integer -> Integer -> Integer -> Shape
checkRectangular a b c | a^2 + b^2 == c^2 = Rectangular
                       | otherwise = Other

checkTriangle :: Integer -> Integer -> Integer -> Shape
checkTriangle a b c | (a + b >= c && a + c >= b && c + b >= a) = Other
                    | otherwise = NoTriangle

checkIsosceles :: Integer -> Integer -> Integer -> Shape
checkIsosceles a b c | (a == b && not (a == c)) = Isosceles
                     | otherwise = Other

testTriangle :: (Int, Int)
testTriangle = ((length results), (length testData))
    where 
        results = filter (==True) $ map (\ ((a, b, c), truth) -> triangle a b c == truth) (zip testData groundTruth)
        testData = [(1, 5, 3), (5, 12, 13), (3, 4, 5), (5, 5, 4), (10, 10, 8), (2, 2, 2), (1,2,3)]
        groundTruth = [NoTriangle, Rectangular, Rectangular, Isosceles, Isosceles, Equilateral, Other]
    
{- 
 - Exercise 3
 - Time: 15 mins
-}

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker   xs p q = stronger xs q p 

-- workshop3a :: Int -> Bool
p1 :: Int -> Bool
p1 x = even x && x > 3

p2 :: Int -> Bool
p2 x = even x || x > 3

p3 :: Int -> Bool
p3 x = even x && x > 3 || even x

p4 :: Int -> Bool
p4 x = even x

{-
A: 
- p1
- p3 and p4
- p2
-}


{- 
 - Exercise 4
 - Time: 30 mins
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | isPermutationRec xs ys == True = True
                    | otherwise = False
        
isPermutationRec xs [] = True
isPermutationRec xs (y:ys) = elem y xs && isPermutationRec xs ys

{- 
- Assuming that the input list cannot contain duplicates makes your 
- precondition more strict than not assuming this. 
- You can make your precondition more strict while maintaining the same properties of the function.
- So if you would make this assumption the function's property should still hold.
-}

{-
- Testable properties (in order of strength):
- Objects represented must be the same
- Length of the list must be the same
- Number of times an object is present in one list is the same as the number of times it is present in the other list
- Above but vice versa
- 
-}
sameObjects :: Eq a => [a] -> [a] -> Bool
sameObjects xs ys = length (xs \\ ys) == 0 && length (xs \\ ys) == 0

sameListLength :: Eq a => [a] -> [a] -> Bool
sameListLength xs ys = length xs ==  length ys

numOccurence :: Eq a => [a] -> [a] -> Bool
numOccurence xs ys = all (==True) [length (findIndices (==x) ys) == length (findIndices (==x) xs) | x <- xs]

strongerPropPerms xs | oneStronger && twoStronger = "Same"
                     | oneStronger = "Length Stronger"
                     | twoStronger = "Object Stronger"
                     | otherwise = "Uncomparable"
                    where
                        oneStronger = stronger testData sameLengthFunc sameObjFunc
                        twoStronger = stronger testData sameObjFunc sameLengthFunc
                        testData = [[1,2,3,4], [4,3,2,1], [5,4,3,2], [1,2,3,4,5]]
                        sameObjFunc = sameObjects xs
                        sameLengthFunc = sameListLength xs
                        sameNumOccFunc = numOccurence xs

                        

{- 
 - Exercise 5
 - Time:  mins
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (==True) [x /= y | (x, y) <- zip xs ys]

deran :: Eq a => [a] -> [[a]]
deran xs = [x | x <- permutations xs, isDerangement x xs]


{- 
 - Exercise 6
 - Time: 
 - A:
 - Rot 13 is a monoalphabetic substitution cipher.
 - This replaces a letter by the letter 13 steps in the alphabet beyond that letter. 
 - To take a few examples, the letter A becomes the letter M, B becomes N. a becomes m and b becomes N.
 - Encrypting and Decrypting is the same operation since the alphabet has 26 characters.
-}
rot13 :: [Char] -> [Char]
rot13 xs = [rot13Single x | x <- xs]

rot13Single :: Char -> Char
rot13Single x | numX >= ord 'a' && numX <= ord 'z' = chr (numX + 13)
              | numX <= ord 'Z' && numX >= ord 'A' = chr (numX + 13)
              | otherwise = x
        where 
            numX = (ord x)