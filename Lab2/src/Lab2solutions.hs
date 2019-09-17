module Lab2solutions where
import Data.List
import Data.Char
import Data.Tuple
import Lab2
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import System.Random
import Numeric

{- 
 - Exercise 1
 - Time: 30 min
-}
quartile :: RealFrac a => [a] -> [Int]
quartile xs = length <$> [[x | x <- xs, x >= low, x < high, x /= 0.0] | (low, high) <- zip [0, 0.25, 0.5, 0.75] [0.25, 0.5, 0.75, 1]]

quartileTest :: Int -> IO [Int]
quartileTest n = quartile <$> probs n

-- Chi Squared testing helper functions --
chiSquareUniformStatistic :: [Int] -> Double
chiSquareUniformStatistic bins = sum ((^2) <$> (subtract expected) <$> fromIntegral <$> bins) / expected
    where expected = fromIntegral (sum bins) / fromIntegral (length bins)

chi2table :: [(Double, Double)]
chi2table = zip [0.35, 0.58, 1.01, 1.42, 2.37, 3.66, 4.64, 6.25, 7.81, 11.34, 16.27] [0.95, 0.9, 0.8, 0.7, 0.5, 0.30, 0.20, 0.10, 0.05, 0.01, 0.001]

chi2p :: Double -> Double
chi2p d = snd (head (filter (\(a, _) -> a > d) chi2table))

-- Chi Square test for uniformity on number of Ints returned by quartileTest
chiSquareTest :: Int -> IO ()
chiSquareTest n = do 
    chi2 <- (chiSquareUniformStatistic <$> quartileTest n)
    putStr "Uniform with probability of at least " 
    putStrLn $ showFFloat (Just 2) (chi2p chi2) ""

{- Since we are testing the uniformity of the amount of numbers in each quartile of a random generator,
 -  we can use a Chi Square test to calculate the consistency of the quartile sizes with a uniform distribution.
 - The above functions calculate the Chi2 statistic assuming a uniform hypothesis and give the highest confirmed
 -  p-value for that statistic (taken from precalculated tables for degrees of freedom = 3 = n - 1).
 - The test returns this p-value given a number of samples, leaving the confidence up to the tester.
 -}

{- 
 - Exercise 2
 - Time: 60 mins
-}
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a' + b' < c' = NoTriangle
    | a' == b' && b' == c' = Equilateral
    | a' ^ 2 + b' ^ 2 == c' ^ 2 = Rectangular
    | a' == b' = Isosceles
    | otherwise = Other
    where [a', b', c'] = sort [a, b, c]

testTriangle :: Bool
testTriangle = all (== NoTriangle) (triangle3 <$> concat (permutations <$> [[1,1,3], [3,3,8], [42, 42, 96]]))
            && all (== Equilateral) (triangle3 <$> concat (permutations <$> [[1,1,1], [3,3,3], [42, 42, 42]]))
            && all (== Rectangular) (triangle3 <$> concat (permutations <$> [[3,4,5], [6,8,10], [10, 24, 26]]))
            && all (== Isosceles) (triangle3 <$> concat (permutations <$> [[1,1,2], [3,3,4], [42, 42, 64]]))
            && all (== Other) (triangle3 <$> concat (permutations <$> [[1,2,3], [2,3,4], [42, 48, 69]]))
            where triangle3 [a, b, c] = triangle a b c

{- 
TODO testreport
-}
{- 
 - Exercise 3
 - Time: 15 mins
-}

forall :: Foldable t => t a -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

property1, property2, property3, property4 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x
property4 = even

{-
    When comparing the properties we give two functions, one specialised for 
        this exercise since the exercise expects us to use the type signature (Int -> Bool) (denoted with sortProperties')
    We also introduce a generic version for later exercises.

    The otherwise statement in the compareProperties' function actually means that they are uncomparable over xs.
    For the purpose of this ordering we think this is sufficient.
-}
compareProperties' :: [a] -> (a -> Bool) -> (a -> Bool) -> Ordering
compareProperties' xs p q
    | stronger xs p q && weaker xs p q = EQ
    | stronger xs p q = GT
    | weaker xs p q = LT
    | otherwise = EQ

sortProperties' :: [a] -> [a -> Bool] -> [Int]
sortProperties' range properties = reverse $ fst <$> sortBy (\(_, a) (_, b) -> compareProperties' range a b) (zip [1..] properties)

testSortProperties :: [Int]
testSortProperties = sortProperties' [-10..10] [property1, property2, property3, property4]
    

-- compareProperties :: [a] -> (a -> Property) -> (a -> Property) -> Ordering
-- compareProperties xs p q
--     | stronger xs p q && weaker xs p q = EQ
--     | stronger xs p q = GT
--     | weaker xs p q = LT
--     | otherwise = EQ

-- sortProperties :: [a] -> [a -> Property] -> [Int]
-- sortProperties range properties = reverse $ fst <$> sortBy (\(_, a) (_, b) -> compareProperties range a b) (zip [1..] properties)
    
-- TODO fix ^^ and redefine stronger && weaker

{-
A: 
- property 1
- property 3 and 4 (are equal)
- property 2
-}


{- 
 - Exercise 4
 - Time: 60 mins
-}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a \\ b == b \\ a
{- 
 - Explanation: using the list difference operator we can find the items only occuring in a or only in b.
 - If there are no items that are only in a and only in b then the list contain the same elements, and are thus permutations of eachother
 - This implementation is duplicate aware, meaning that a list with one or more duplicates can never be a permutation of a list without duplicates.
 -}

{- 
- Assuming that the input list cannot contain duplicates makes your 
-  precondition stricter than not assuming this. 
- Preconditions can be made stricter without affecting the properties of the function.
- So if you would assume no duplicates (and reflect it in the precondition) 
  the function's properties should still hold.
-}

{-
- Testable properties (in order of strength):
- Objects represented must be the same
- Length of the list must be the same
- Number of times an object is present in one list is the same as the number of times it is present in the other list
- Above but vice versa
- 
-}
prop_sameListLengthPerm :: Eq a => ([a], [a]) -> Property
prop_sameListLengthPerm (xs, ys) = isPermutation xs ys ==> length xs == length ys

prop_numOccurencePerm :: Eq a => [a] -> [a] -> Bool
prop_numOccurencePerm xs ys = isPermutation xs ys --> all (==True) [length (findIndices (==x) ys) == length (findIndices (==x) xs) | x <- xs]

{-
    We want to test our properties using quickCheck.
    If we just run the normal quickcheck (quickCheck <propertyfunction>) 
        The number of testcases relevant to our function is limited.
    This is why instead of the --> operator we use the ==> which acts as a 
        precondition for our propertytester. This way we only test actual permutations for this property.
    But now we discard most of our testcases because most generated lists are not actually permutations.
    This is why wrote our own generator which generates a tuple of the a fewer amount of integers so the 
        chance of having a permutation is higher.
    We noticed that the tests then still take an awful long amount of time.
    So what we did is limit the length of the list to 5, so the tests took less time.
    Now we still have a large number of test cases that are discarded but at least it's faster.
    Not we want to use the permutations function to generate permutations.
-}

genSmallRangeListTuple :: Gen ( [Int], [Int])
genSmallRangeListTuple = liftM2 (,) (listOf (choose (1,3))) (listOf (choose (1,3)))

{-testPermProps :: IO ()
testPermProps = do
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_sameListLengthPerm)
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_numOccurencePerm)
    quickCheck prop_sameListLengthPerm
    quickCheck prop_numOccurencePerm
-}
{- 
 - Exercise 5
 - Time: 60 mins
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (==True) [x /= y | (x, y) <- zip xs ys]

deran :: (Eq a, Num a, Enum a) => a -> [[a]]
deran n = [x | x <- permutations xs, isDerangement x xs]
    where xs = [0..n-1]
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
AND WITH QUICKCHECK SO CHANGE IT TO PROPERTIES

testDeranProps :: IO ()
testDeranProps = do
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_sameObjectsDeran)
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_sameListLengthDeran)
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_numOccurenceDeran)
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_samePosDeran)
    verboseCheck (withMaxSuccess 100 $ forAll (resize 5 genSmallRangeListTuple) prop_unequalDeran)
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
rot13 s = rot13Single <$> s

rot13Single :: Char -> Char
rot13Single s 
    | s `elem` (['A'..'M']++['a'..'m']) = chr (ord s + 13)
    | s `elem` (['N'..'Z']++['n'..'z']) = chr (ord s - 13)
    | otherwise = s
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

prop_lengthRot13 :: String -> Bool
prop_lengthRot13 xs = length xs == length (rot13 xs)

prop_sameCaseRot13 :: String -> Bool
prop_sameCaseRot13 xs = map toLower (rot13 xs) == rot13 (map toLower xs)

-- TODO CONVERT TO PROPERTIES!

testRot13Props :: IO ()
testRot13Props = do
    quickCheck prop_doubleRot13
    quickCheck prop_upperCaseRot13
    quickCheck prop_sameCaseRot13
    quickCheck prop_lengthRot13

{- 
 - Exercise 7
 - Time: 120 min
 - Check IBAN numbers by checking the checksum and verifying the length with the country code.
 -}
ibanChrToNums :: Char -> String
ibanChrToNums c 
    | isAlpha c = show $ ord c - ord 'A' + 10
    | isNumber c = [c]

ibanReplaceLetters :: String -> String
ibanReplaceLetters s = concat $ ibanChrToNums <$> s

ibanMoveCharacters :: String -> String
ibanMoveCharacters s = drop 4 s ++ take 4 s

ibanToInteger :: String -> Integer
ibanToInteger = read . ibanReplaceLetters . ibanMoveCharacters

countryCodes = ["AX", "AL", "AD", "AT", "AZ", "BH", "BY", "BE", "BA", "BR", "BG", 
                "CR", "HR", "CY", "CZ", "DK", "DO", "SV", "EE", "FO", "FI", "FR", 
                "GE", "DE", "GI", "GR", "GL", "GT", "VA", "HU", "IS", "IQ", "IE", 
                "IL", "IT", "JO", "KZ", "XK", "KW", "LV", "LB", "LI", "LT", "LU", 
                "MT", "MR", "MU", "MD", "MC", "ME", "NL", "MK", "NO", "PK", "PS", 
                "PL", "PT", "QA", "RO", "LC", "SM", "ST", "SA", "RS", "SC", "SK", 
                "SI", "ES", "SE", "CH", "TL", "TN", "TR", "UA", "AE", "GB", "VG"]

countryLength = [18, 28, 24, 20, 28, 22, 28, 16, 20, 29, 22, 22, 21, 28, 24, 18, 
                 28, 28, 20, 18, 18, 27, 22, 22, 23, 27, 18, 28, 22, 28, 26, 23, 
                 22, 23, 27, 30, 20, 20, 30, 21, 28, 21, 20, 20, 31, 27, 30, 24,
                 27, 22, 18, 19, 15, 24, 29, 28, 25, 29, 24, 32, 27, 25, 24, 22,
                 31, 24, 19, 24, 24, 21, 23, 24, 26, 29, 23, 22, 24]

getCountryLength :: String -> Maybe Int
getCountryLength s = (countryLength !!) <$> elemIndex s countryCodes

ibanCheckCountrySize :: String -> Bool
ibanCheckCountrySize s = maybe False (length s ==) (getCountryLength (take 2 s))

iban :: String -> Bool
iban s = ibanCheckCountrySize s && ibanToInteger s `mod` 97 == 1

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



{- 
 - BONUS:

    Euler 182:
    A: 399788195976
-}


euler182 :: Integer
euler182 = sum [e | e <- [0..totient-1], concealedp !! fromIntegral (e `mod` (p-1)) == concealedpmin, concealedq !! fromIntegral (e `mod` (q-1)) == concealedqmin]
    where 
          p = 1009
          q = 3643
          n = p * q
          totient = (p-1)*(q-1)
          concealedp = euler182_all_unconcealed p
          concealedq = euler182_all_unconcealed q
          concealedpmin = minimum concealedp
          concealedqmin = minimum concealedq

euler182_all_unconcealed :: Integer -> [Integer]
euler182_all_unconcealed p = [ if gcd e (p-1) == 1 then euler182_unconcealed p e else 10^10 | e <- [0..p-2]]

euler182_unconcealed :: Integer -> Integer -> Integer
euler182_unconcealed modulo e = fromIntegral $ length $ filter(\ m -> m ^ e `mod` modulo == m) [0..modulo-1]

{- 
    Euler 97:
    A: 8739992577
-}

euler97 :: String
euler97 = reverse $ take 10 (reverse (show (28433*2^7830457+1)))


{- 
    Euler 92:
    A: 8581146
-}

euler92 :: Int
euler92 = length $ filter (== True) [euler92Helper x | x <- [1..10^7-1]]

euler92Helper :: Integer -> Bool
euler92Helper 89 = True
euler92Helper 1 = False
euler92Helper x = euler92Helper $ sum [fromIntegral (digitToInt y)^2 | y <- show x]