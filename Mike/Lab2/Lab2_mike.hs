
module Lab2_mike where
import Lab2
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck

-- 1 hr --
-- Based on : https://stackoverflow.com/questions/52309869/convert-io-float-to-float-in-haskell
measureQuartiles :: IO [Int]
measureQuartiles = fmap (\x -> measureQuartile [0, 0.25, 0.5, 0.75, 1] x) $ probs 10000

measureQuartile :: Ord a => [a] -> [a] -> [Int]
measureQuartile [x] _ = []
measureQuartile (q1:q2:qs) xs = (length (filter (\x -> x > q1 && x < q2) xs)):measureQuartile (q2:qs) xs


-- 10 min --
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
            | a + b <= c || a + c <= b || b + c <= a = NoTriangle
            | a == b && a == c && b == c = Equilateral
            | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular
            | (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a) = Isosceles
            | otherwise = Other


-- [p1, p3, p4, p2]
-- 20 min
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1 :: Int -> Bool
p1 x = even x && x > 3
--
p2 :: Int -> Bool
p2 x = even x || x > 3

p3 :: Int -> Bool
p3 x = (even x && x > 3) || even x

p4 :: Int -> Bool
p4 x = even x

-- 30 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | isPermutationRec xs ys == True = True
                    | otherwise = False

isPermutationRec xs [] = True
isPermutationRec xs (y:ys) = elem y xs && isPermutationRec xs ys

equalElements [] ys = True
equalElements (x:xs) ys = x elem ys && equalElements xs ys

equalLength x y = length x == length y



--20 --
isDerangement xs ys | isPermutation xs ys = isDerangementRec xs ys
                    | otherwise = False

isDerangementRec [] [] = True
isDerangementRec (x:xs) (y:ys) = x /= y && isDerangementRec xs ys

deran xs = [x | x <- (permutations xs), isDerangement x xs]

-- 25 min --

rot13spec :: [Char] -> [Char]
rot13spec cs = map monoAlphabetic cs

monoAlphabetic :: Char -> Char
monoAlphabetic c | ordC >= ord 'a' && ordC < ord 'n' = chr (ordC + 13)
                 | ordC >= ord 'A' && ordC < ord 'N' = chr (ordC + 13)
                 | ordC >= ord 'n' && ordC <= ord 'z' = chr $ (ordC + 13) `mod` 123 + 97
                 | ordC >= ord 'N' && ordC <= ord 'Z' = chr $ (ordC + 13) `mod` 91 + 65
        where
            ordC = ord c

-- The decryption is done by applying the encryption again: E(E(x)) = x -> D(x) = E(x)
rot13dec :: [Char] -> Bool
rot13dec cs = rot13spec (rot13spec cs) == cs


-- 60 min --

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

checkLengths :: [Char] -> Int -> Bool
checkLengths cc l = countryLength !! fromMaybe 0 (elemIndex cc countryCodes) == l

checkModulo :: [Char] -> Bool
checkModulo (x1:x2:x3:x4:xs) = mod ((read $ concat $ map (show) (map (\x -> ord x - 55) (take 4 xs) ++ newL)) :: Integer) 97 == 1
                            where
                                s1 = ord x1 - 55
                                s2 = ord x2 - 55
                                s3 = digitToInt x3
                                s4 = digitToInt x4
                                newxs = map digitToInt (drop 4 xs)
                                newL = newxs++[s1]++[s2]++[s3]++[s4]


iban :: String -> Bool
iban s | checkLengths (take 2 s) (length s) && checkModulo s = True
       | otherwise = False
