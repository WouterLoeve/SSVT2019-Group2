import Lab2
import Test.QuickCheck
import Data.List
import Data.Char
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map

-- Redefinition of forall, Lab2.hs is wrong
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Excercise 1: random in open interval, time spent: 1 hour
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

-- The quartiles roughly follow the expected distribution of [2500, 2500, 2500, 2500]

-- Excercise 2: recognizing Triangles, time spent: 1.5 hour
findTriangle :: Integer -> Integer -> Integer -> Shape
findTriangle a b c | (c >= (a+b)) = NoTriangle
                   | ((a==b) && (b==c)) = Equilateral
                   | (a^2 + b^2 == c^2) = Rectangular
                   | ((a==b)||(a==c)||(b==c)) = Isosceles
findTriangle _ _ _ = Other

-- Make param C the largest
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | (z >= x) && (z >= y) = findTriangle x y z
               | (y >= x) && (y >= z) = findTriangle x z y
               | (x >= y) && (x >= z) = findTriangle y z x

-- helper function that accepts list instead of params
triangle' :: [Integer] -> Shape
triangle' [x,y,z] = triangle x y z

-- helper function that tests all permutations of a list of parameters against expected output
testTriangleHelper l e =
   let p = permutations l in
      all (==True) [(triangle' i) == e | i <- p]
    
-- test function that tests permutations of known results
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

-- Excercise 3: Testing properties strength, time spent: 30 mins
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

propEven x = even x 
prop1 x = (even x && x > 3)
prop2 x = (even x || x > 3)
prop3And4 x = ((even x && x > 3) || even x)

{-
 - ==STRONGEST==
 - prop1
 - propEven
 - prop3And4
 - prop2
 - ==WEAKEST==
 -}

-- Excercise 4: Recognizing Permutations, time spent: 30 mins
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = null (a \\ b) && null (b \\ a)
-- explanation: using the list difference operator we can find the items only occuring in a or only in b.
-- If there are no items that are only in a or only in b then the list contain the same elements, and are thus permutations of eachother
-- This implementaiton is duplicate aware, meaning that a list with one or more duplicates can never be a permutation of a list without duplicates.

-- Requirements
-- 1. length a & b must be the same
-- 2. The types must derive Eq
-- It is assumed that lists in Haskell can only contain a single type

-- Properties of permutations
-- 1. list a and permutation b have the same length
-- 2. list a and permutation b have the same amount of each element (e.g. two occurences of 5 in each)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

prop_permutationLength a b = length a == length b
prop_permutationElements a b = all (==True) [count x a == count x b | x <- blend a b]


testIsPermutation =
    (isPermutation [1,2,3] [2,1,3]) &&
    not (isPermutation [1,2,3] [2,4,3]) &&
    (isPermutation [1.5,2.4,3.3] [2.4,1.5,3.3]) &&
    not (isPermutation [1.5,2.4,3.3] [2.4,1.3,3.5])

-- Exercise 5: Recognizing and generating derangements, time spent: 1 hour
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && all (==True) [x/=y | (x,y)<-(zip a b)]

deran :: (Eq a, Num a, Enum a) => a -> [[a]]
deran 0 = []
deran 1 = []
deran n = let source = [0..(n-1)]
              mutations = permutations source in
    [x | x<-mutations, isDerangement source x]

-- properties isDerangement:
-- 1. list must be equal length
-- 2. lists must be permutations of eachother
-- The properties can be ranked as follows:
listLength x y = length x == length y
curryPermutation n = isPermutation [1,2,3,4] n
curryLength n = listLength [1,2,3,4] n
-- stronger [[2,3,4,1], [5,2,1,3]] curryPermutation curryLength
--
-- The result here is (in order from strongest to weakest):
-- permutation
-- list length

-- Exercise 6: Implementing and testing ROT13 encoding, time spent: 45 mins
-- Specification
-- ROT13 maps an input character in the range [A..Z]++[a..z] to 
-- an output character in the range [N..Z]++[A..M]++[n..z]++[a..m]
-- an input character outside of this range will remain the same.
rot13' s | s `elem` (['A'..'M']++['a'..'m']) = chr (ord s + 13)
         | s `elem` (['N'..'Z']++['n'..'z']) = chr (ord s - 13)
         | otherwise = s
rot13 s = [rot13' c | c <- s]

{-
 - Properties:
 - 1. Rot13 is it's own inverse. Running rot13 twice returns the original string.
 - 2. The string length of input and rot13-encoded output is the same
 - 3. Rot13 preserves uppercase during encoding.
 -}
prop_rot13Inverse s = rot13 (rot13 s) == s
prop_rot13Length s = length s == length (rot13 s)
prop_rot13Upper s = all (==True) [isUpper c == isUpper (rot13' c) | c <- s]



-- Excercise 7: implementing IBAN, time spent: 1 hour
--iban :: String -> Bool
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

ibanCheckHelper s = let q = [fst a | a <- zip countryLength countryCodes, snd a == (take 2 s)] in
    (length s) == (if length q == 0 then 0 else head q)

ibanParseHelper s = let inp = [if isLetter c then ord (toUpper c) - 55 else digitToInt c | c <- s] in
    read (intercalate "" ([show a | a <- inp])) :: Integer

iban inp | ibanCheckHelper inp = let num = filter (/=' ') inp in
    let s = ibanParseHelper (filter (/=' ') ((drop 4 num) ++ (take 4 num))) in
    (s `mod` 97) == 1
         | otherwise = False

testIbanNums = ["AX2112345600000785", "AL47212110090000000235698741", "AD1200012030200359100100", "AD1200012030200359100100", "AT611904300234573201", "BY13NBRB3600900000002Z00AB00", "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160", "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685", "FO2000400440116243", "FI2112345600000785", "FR1420041010050500013M02606", "DE89370400440532013000", "GI75NWBK000000007099453", "GR1601101250000000012300695", "GL2000400440116243", "HU42117730161111101800000000", "IS140159260076545510730339", "IE29AIBK93115212345678", "IT60X0542811101000000123456", "XK051212012345678906", "LV80BANK0000435195001", "LI21088100002324013AA", "LU280019400644750000", "MK07250120000058984", "MT84MALT011000012345MTLCAST001S", "MD24AG000225100013104168", "MC5811222000010123456789030", "ME25505000012345678951", "NL91ABNA0417164300", "NO9386011117947", "PL61109010140000071219812874", "PT50000201231234567890154", "RO49AAAA1B31007593840000", "SM86U0322509800000000270100", "RS35260005601001611379", "SK3112000000198742637541", "ES9121000418450200051332", "SI56191000000123438", "SE4550000000058398257466", "CH9300762011623852957", "GB29NWBK60161331926819", "AZ21NABZ00000000137010001944", "BH67BMAG00001299123456", "BR1800000000141455123924100C2", "VG96VPVG0000012345678901"]

testIban = 
    all (==True) [iban x | x <- testIbanNums]
