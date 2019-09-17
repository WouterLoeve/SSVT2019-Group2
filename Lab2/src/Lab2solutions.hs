module Lab2solutions where
import Data.List
import Data.Char
import Data.Tuple
import Lab2
import Text.Show.Functions
import Test.QuickCheck
-- import Test.QuickCheck.Instances.Tuple

{- 
 - Exercise 1
 - Time: 30 min
 - A: In each quartile there are about 2500 items. Variance report?
-}


quartile :: RealFrac a => [a] -> [Int]
quartile xs = [length $ filter (== y) [ ceiling (x * 4) | x <- xs ] | y <- [1..4]]

quartileTest :: IO [Int]
quartileTest = fmap quartile (probs 10000)

{- 
 - Exercise 2
 - Time: 60 mins
-}
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | checkTriangle a b c == NoTriangle = NoTriangle
               | null checkFilter = Other
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
checkEquilateral a b c | a == b && b == c = Equilateral
                       | otherwise = Other

checkRectangular :: Integer -> Integer -> Integer -> Shape
checkRectangular a b c | a^2 + b^2 == c^2 = Rectangular
                       | otherwise = Other

checkTriangle :: Integer -> Integer -> Integer -> Shape
checkTriangle a b c | a + b >= c && a + c >= b && c + b >= a = Other
                    | otherwise = NoTriangle

checkIsosceles :: Integer -> Integer -> Integer -> Shape
checkIsosceles a b c | a == b && a /= c = Isosceles
                     | otherwise = Other

testTriangle :: (Int, Int)
testTriangle = (length results, length testData)
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
p4 = even

{-
A: 
- p1
- p3 and p4
- p2
-}


{- 
 - Exercise 4
 - Time: 60 mins
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | foldr (\y -> (&&) (y `elem` xs)) True ys = True
                    | otherwise = False
        
-- isPermutationRec xs [] = True
-- isPermutationRec xs (y:ys) = elem y xs && isPermutationRec xs ys

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
prop_sameObjectsPerm :: ([Int], [Int]) -> Bool
prop_sameObjectsPerm (xs, ys) = isPermutation xs ys --> xs \\ ys == ys \\ xs

prop_sameListLengthPerm :: Eq a => [a] -> [a] -> Bool
prop_sameListLengthPerm xs ys = isPermutation xs ys --> length xs == length ys

prop_numOccurencePerm :: Eq a => [a] -> [a] -> Bool
prop_numOccurencePerm xs ys = isPermutation xs ys --> all (==True) [length (findIndices (==x) ys) == length (findIndices (==x) xs) | x <- xs]


-- -- prop_test :: [Integer] -> [Integer] -> Property
-- prop_test xs = forAll listOf(choose (1,5)) $ \ xs ys -> isPermutation xs ys ==> length xs == length ys


-- genSmallRangeList :: Gen [Int]
-- genSmallRangeList = (listOf (choose (1,5)))

-- genSmallRangeListTuple :: Gen ( [Int], [Int])
-- genSmallRangeListTuple = (><) (listOf (choose (1,5))) (listOf (choose (1,5)))

{- 
 * Exercise 1 
 * Commands: quickCheck $ forAll genPositiveIntegers testSumSquares
 * 
-}

-- strongerPropPerms xs | oneStronger && twoStronger = "Same"
--                      | oneStronger = "Length Stronger"
--                      | twoStronger = "Object Stronger"
--                      | otherwise = "Uncomparable"
--                     where
--                         oneStronger = stronger testData sameLengthFunc sameObjFunc
--                         twoStronger = stronger testData sameObjFunc sameLengthFunc
--                         testData = [[1,2,3,4], [4,3,2,1], [5,4,3,2], [1,2,3,4,5]]
--                         sameObjFunc = prop_sameObjectsPerm xs
--                         sameLengthFunc = prop_sameListLengthPerm xs
--                         sameNumOccFunc = prop_numOccurencePerm xs

-- testPermProps :: IO ()
-- testPermProps = do
--     quickCheck prop_sameObjectsPerm
--     quickCheck prop_sameListLengthPerm
--     quickCheck prop_numOccurencePerm

{- 
 - Exercise 5
 - Time: 60 mins
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (==True) [x /= y | (x, y) <- zip xs ys]

deran :: Eq a => [a] -> [[a]]
deran xs = [x | x <- permutations xs, isDerangement x xs]


{-
 - A derangement is a stronger property than permutation since it is a special case of the permutation.
 - That means the same properties of permutation also hold.
 - Additionally 
    - there is also the property of a derangement that an element doesn't occur on the same position
    - A derangement is never the same as the orignal list, unless that list is empty
 
-}
prop_sameObjectsDeran :: Eq a => [a] -> [a] -> Bool
prop_sameObjectsDeran xs ys = isDerangement xs ys --> xs \\ ys == ys \\ xs

prop_sameListLengthDeran :: Eq a => [a] -> [a] -> Bool
prop_sameListLengthDeran xs ys = isDerangement xs ys --> length xs == length ys

prop_numOccurenceDeran :: Eq a => [a] -> [a] -> Bool
prop_numOccurenceDeran xs ys = isDerangement xs ys --> all (==True) [length (findIndices (==x) ys) == length (findIndices (==x) xs) | x <- xs]

prop_samePosDeran :: Eq a => [a] -> [a] -> Bool
prop_samePosDeran xs ys = isDerangement xs ys --> all (==True) [x /= y | (x, y) <- zip xs ys]

prop_unequalDeran :: Eq a => [a] -> [a] -> Bool
prop_unequalDeran xs ys = isDerangement xs ys && not (null xs) --> xs /= ys
{-
TODO TEST USING DERAN GENERATOR????
-}

{- 
 - Exercise 6
 - Time: 30
 - A:
 - Rot 13 is a monoalphabetic substitution cipher.
 - This replaces a letter by the letter 13 steps in the alphabet beyond that letter. 
 - To take a few examples, the letter A becomes the letter N, B becomes O. a becomes n and b becomes o.
 - Encrypting and Decrypting is the same operation since the alphabet has 26 characters.
-}
rot13 :: String -> String
rot13 = map rot13Single

rot13Single :: Char -> Char
rot13Single x | numX >= ord 'a' && numX < ord 'n' = chr (numX + 13)
              | numX >= ord 'n' && numX <= ord 'z' = chr (numX - 13)
              | numX >= ord 'A' && numX < ord 'N' = chr (numX + 13)
              | numX >= ord 'N' && numX <= ord 'Z' = chr (numX - 13)
              | otherwise = x
        where 
            numX = ord x

{-
- Testable properties:
    Encrypting is the same as decrypting
    Uppercase are always mapped to uppercase and lowercase always to lower
    Converting the string to lowercase before rot13 and after rot13 have the same result (associative)
        (e.g. toLower rot13(Z) == rot13(z))
-}
prop_doubleRot13 :: String -> Bool
prop_doubleRot13 xs = rot13 (rot13 xs) == xs

prop_upperCaseRot13 :: String -> Bool
prop_upperCaseRot13 xs = map isUpper (rot13 xs) == map isUpper xs

prop_sameCaseRot13 :: String -> Bool
prop_sameCaseRot13 xs = map toLower (rot13 xs) == rot13 (map toLower xs)

testRot13Props :: IO ()
testRot13Props = do
    quickCheck prop_doubleRot13
    quickCheck prop_upperCaseRot13
    quickCheck prop_sameCaseRot13

-- prop_
{- 
 - Exercise 7
 - Time: 120 min
-}

countryCodes = ["AX", "AL", "AD", "AT", "AZ", "BH", "BY", "BE", "BA", "BR", "BG", 
                "CR", "HR", "CY", "CZ", "DK", "DO", "SV", "EE", "FO", "FI", "FR", 
                "GE", "DE", "GI", "GR", "GL", "GT", "VA", "HU", "IS", "IQ", "IE", 
                "IL", "IT", "JO", "KZ", "XK", "KW", "LV", "LB", "LI", "LT", "LU", 
                "MT", "MR", "MU", "MD", "MC", "ME", "NL", "MK", "NO", "PK", "PS", 
                "PL", "PT", "QA", "RO", "LC", "SM", "ST", "SA", "RS", "SC", "SK", 
                "SI", "ES", "SE", "CH", "TL", "TN", "TR", "UA", "AE", "GB", "VG"]

countryLength = [18, 28, 24, 20, 28, 22, 28, 16, 20, 29, 22, 22, 21, 28, 24, 18, 28, 
                 28, 20, 18, 18, 27, 22, 22, 23, 27, 18, 28, 22, 28, 26, 23, 22,
                 23, 27, 30, 20, 20, 30, 21, 28, 21, 20, 20, 31, 27, 30, 24, 27,
                 22, 18, 19, 15, 24, 29, 28, 25, 29, 24, 32, 27, 25, 24, 22, 31,
                 24, 19, 24, 24, 21, 23, 24, 26, 29, 23, 22, 24]

getCountryLength :: String -> Integer
getCountryLength code = snd $ head (filter (\ (x, len) -> x == code) $ zip countryCodes countryLength)

concatDigits :: Integer -> Integer -> Integer
concatDigits a b | b >= 10 && b < 100 = a * 100 + b
                 | b < 10 = a * 10 + b
                 | otherwise = a * b

handleBBAN :: String -> Integer
handleBBAN x = foldl concatDigits 0 (map convertCharsToIBANInts (b ++ a))
    where 
        (a, b) = splitAt 4 x

convertCharsToIBANInts :: Num p => Char -> p
convertCharsToIBANInts x | isDigit x = fromIntegral (digitToInt x)
                         | otherwise = fromIntegral (ord x - ord 'A' + 10)
                        
iban :: String -> Bool
iban x | countryCode `notElem` countryCodes = False
       | getCountryLength countryCode /= fromIntegral (length x) = False
       | handleBBAN x `mod` 97 == 1 = True
        where countryCode = fst (splitAt 2 x)

{- 
 - Testing:
-}
ibanCorrectTests :: (Int, Int)
ibanCorrectTests = (length $ filter (==True) (map iban testData), length testData)
    where testData = ["AX2112345600000785", "AL47212110090000000235698741", "AD1200012030200359100100", "AD1200012030200359100100", "AT611904300234573201", "BY13NBRB3600900000002Z00AB00", "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160", "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685", "FO2000400440116243", "FI2112345600000785", "FR1420041010050500013M02606", "DE89370400440532013000", "GI75NWBK000000007099453", "GR1601101250000000012300695", "GL2000400440116243", "HU42117730161111101800000000", "IS140159260076545510730339", "IE29AIBK93115212345678", "IT60X0542811101000000123456", "XK051212012345678906", "LV80BANK0000435195001", "LI21088100002324013AA", "LU280019400644750000", "MK07250120000058984", "MT84MALT011000012345MTLCAST001S", "MD24AG000225100013104168", "MC5811222000010123456789030", "ME25505000012345678951", "NL91ABNA0417164300", "NO9386011117947", "PL61109010140000071219812874", "PT50000201231234567890154", "RO49AAAA1B31007593840000", "SM86U0322509800000000270100", "RS35260005601001611379", "SK3112000000198742637541", "ES9121000418450200051332", "SI56191000000123438", "SE4550000000058398257466", "CH9300762011623852957", "GB29NWBK60161331926819", "AZ21NABZ00000000137010001944", "BH67BMAG00001299123456", "BR1800000000141455123924100C2", "VG96VPVG0000012345678901"]

-- ibanInCorrectLength :: (Int, Int)
-- ibanInCorrectLength = (length $ filter (==True) (map iban testData), length testData)
--     where testData = ["AX2112345600000785", "AL47212110090000000235698741", "AD1200012030200359100100", "AD1200012030200359100100", "AT611904300234573201", "BY13NBRB3600900000002Z00AB00", "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160", "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685", "FO2000400440116243", "FI2112345600000785", "FR1420041010050500013M02606", "DE89370400440532013000", "GI75NWBK000000007099453", "GR1601101250000000012300695", "GL2000400440116243", "HU42117730161111101800000000", "IS140159260076545510730339", "IE29AIBK93115212345678", "IT60X0542811101000000123456", "XK051212012345678906", "LV80BANK0000435195001", "LI21088100002324013AA", "LU280019400644750000", "MK07250120000058984", "MT84MALT011000012345MTLCAST001S", "MD24AG000225100013104168", "MC5811222000010123456789030", "ME25505000012345678951", "NL91ABNA0417164300", "NO9386011117947", "PL61109010140000071219812874", "PT50000201231234567890154", "RO49AAAA1B31007593840000", "SM86U0322509800000000270100", "RS35260005601001611379", "SK3112000000198742637541", "ES9121000418450200051332", "SI56191000000123438", "SE4550000000058398257466", "CH9300762011623852957", "GB29NWBK60161331926819", "AZ21NABZ00000000137010001944", "BH67BMAG00001299123456", "BR1800000000141455123924100C2", "VG96VPVG0000012345678901"]


-- ibanIncorrectCountryCode

-- TODO: Testing && property lists