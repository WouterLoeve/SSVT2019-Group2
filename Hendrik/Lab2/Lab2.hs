module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Stock --
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- 1 --
intervals :: Int -> IO ()
intervals n = do
    print =<< length <$> filter (>= 0.00) <$> filter (< 0.25) <$> (probs n)
    print =<< length <$> filter (>= 0.25) <$> filter (< 0.50) <$> (probs n)
    print =<< length <$> filter (>= 0.50) <$> filter (< 0.75) <$> (probs n)
    print =<< length <$> filter (>= 0.75) <$> filter (< 1.00) <$> (probs n)

-- 2 / 30m--
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

-- 3 --
forall :: Foldable t => t a -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

property1, property2, property3 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x

compareProperties :: [a] -> (a -> Bool) -> (a -> Bool) -> Ordering
compareProperties xs p q
    | stronger xs p q = GT
    | weaker xs p q = LT
    | otherwise = EQ

propertyList :: [(Int -> Bool)]
propertyList = sortBy (compareProperties [-10..10]) [property1, property2, property3]

-- 4 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation a b = length a == length b
    && isPermutation (drop 1 a) (delete (a !! 0) b)

-- 5 --
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b
    && all (\e -> e `elemIndex` a /= e `elemIndex` b) a

deran :: Eq a => [a] -> [[a]]
deran a = filter (\pi -> all (\e -> e `elemIndex` a /= e `elemIndex` pi) a) (permutations a)

-- 6 --
{- rot13 takes a string and returns a string of the same length with:
 -  each letter replaced with a letter 13 places up in the alphabet
 -  (in the same case) s.t. rot13 a = n, rot13 A = N, and rot13 rot13 a = a
 -}

rot13chr :: Char -> Char
rot13chr a 
    | a `elem` ['A'..'Z'] = chr $ ord 'A' + (ord a - ord 'A' + 13) `mod` 26
    | a `elem` ['a'..'z'] = chr $ ord 'a' + (ord a - ord 'a' + 13) `mod` 26

rot13 :: String -> String
rot13 a = rot13chr <$> a

-- 7 --
ibanChrToNums :: Char -> String
ibanChrToNums c 
    | isAlpha c = show $ ord c - ord 'A' + 10
    | isNumber c = [c]

ibanReplaceLetters :: String -> String
ibanReplaceLetters s = concat $ ibanChrToNums <$> s

ibanMoveCharacters :: String -> String
ibanMoveCharacters s = drop 4 s ++ take 4 s

ibanToInteger :: String -> Integer
ibanToInteger s = (read . ibanReplaceLetters . ibanMoveCharacters) s

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
iban s = ibanCheckCountrySize s && ibanToInteger s `mod` 79 == 1