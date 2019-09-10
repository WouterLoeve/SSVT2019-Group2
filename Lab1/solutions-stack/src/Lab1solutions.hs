module Lab1solutions where
import Lab1
import Data.List
import Test.QuickCheck
import Data.Char
import Text.JSON.Generic
import Text.Printf

genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

{- 
 * Exercise 1 
 * Commands: quickCheck $ forAll genPositiveIntegers testSumSquares
 * 
-}

sumSquares' :: Int -> Int
sumSquares' n = sum [ k^2 | k <- [1..n] ]

sumSquares :: Int -> Int
sumSquares n = quot (n*(n + 1)*(2*n + 1)) 6

testSumSquares :: Int -> Bool
testSumSquares n = sumSquares' n == sumSquares n

{- 
 * Part 2
 * Commands: quickCheck $ forAll genPositiveIntegers testSumCubes
 * 
-}

sumCubes' :: Int -> Int
sumCubes' n = sum [ k^3 | k <- [1..n] ]

sumCubes :: Int-> Int
sumCubes n = (^) (quot (n*(n + 1)) 2) 2

testSumCubes :: Int -> Bool
testSumCubes n = sumCubes' n == sumCubes n


{- 
 * Exercise 2
-}

testPowerCardinality :: Int -> Bool
testPowerCardinality n = length (subsequences [1..n]) == 2^n

{- 
 * This property is hard to test because for large number the function runs for a long time. 
 * There is also an infinite number of natural numbers to test this with. 
 * This makes it impossible to test the function fully with random tests. 
 * At this point you should also ask yourself wether you are testing the library functions or your own code. 
 * So to conclude what you are testing is wether the function gives correct output on the things you put into it. 
 * The result of this test is should be the conclusion of that your code works practically, not formally.
-}

{- 
 * Exercise 3
-}

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fac :: Int -> Int
fac 0 = 1
fac n = product [1..n]

testPermsCardinality :: Int -> Bool
testPermsCardinality n = length (perms [1..n]) == fac n

{-
 * This property is also hard to test because of the same reason.
 * Computation time exceeds exponentially with increasing n.
 * When testing this function you are not only dependent on a correct implementation of the perm function.
 * But also on a correct implementation of your own function. 
 * Just like the previous exercise it is impossible to formally test the property 
 *  because there is an infinite amount of numbers that is to be tested. 
-}

{- 
 * Exercise 4
-}
reversiblePrimes :: [Integer]
reversiblePrimes = takeWhile (< 10000) (filter (prime . reversal) primes)

{-
 * Either find a library implementation which you are sure works
 * Find the list that statisfies these condtions on the internet
 * Or do it by hand
 * Do note that all of these methods are very sub optimal. 
 * You can't really be sure of that the library function works fully without proving it mathematically.
 * It's possible that this list is not available online.
 * Doing it by hand would logically be very tiresome.
-}

{- 
 * Exercise 5
-}

sum101Primes :: Int -> Integer
sum101Primes n = sum (take 101 (drop n primes))

smallestConsecutivePrime :: Integer
smallestConsecutivePrime = head (filter prime (map sum101Primes [1..]))

{-
 * Answer: 37447
 * There doesn't seem to be a way to test this as the implementation
 *  of the test can at best be as accurate as the original
 *  (i.e. a different implementation or by hand).
-}

{- 
 * Exercise 6
-}
productPlusOneNotPrime :: [Integer] -> Bool
productPlusOneNotPrime l = not (prime (product l + 1))

smallestCounterExample :: [Integer]
smallestCounterExample = head (filter productPlusOneNotPrime (map (`take` primes) [1..]))

{- 
 * 30031 is the smallest counterexample, this is the list [2,3,5,7,11,13]
-}


{- 
 * Exercise 7
-}
digits :: Integer -> [Integer]
digits = map (read . return) . show

luhndouble :: [Integer] -> [Integer]
luhndouble l = [if odd (snd x) then fst x else sum (digits (fst x * 2)) | x <- zip (reverse l) [1..]]

luhn :: Integer -> Bool
luhn n = sum (luhndouble (digits n)) `mod` 10 == 0

checkIIN :: [Integer] -> [Integer] -> Bool
checkIIN prefixes list = any (`isPrefixOf` list) (map digits prefixes)

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n
    && length l == 15 
    && checkIIN [34, 37] l 
    where l = digits n

isMaster :: Integer -> Bool
isMaster n = luhn n
    && length l == 16
    && checkIIN ([2221 .. 2720] ++ [51 .. 55]) l 
    where l = digits n

isVisa :: Integer -> Bool
isVisa n = luhn n
    && length l == 16 
    && checkIIN [4] l 
    where l = digits n

{- 
 * Tests
-}

testMastercardCorrect :: String
testMastercardCorrect = 
    let nums = [5569529620568055, 5472887618656159, 
                5236849953749038, 5203832230045379, 
                5164860703923613, 5197557698575371, 
                5204240503838299, 5362977367866993, 
                5509016394433883, 5307347394793966]
    in printf "Validated Correct MasterCard test %d / %d" (length (filter (==True) [isMaster x | x <- nums])) (length nums)

testMastercardWrong :: String    
testMastercardWrong = 
    let nums = [5569529620568054, 5472887618656158, 
                5236849953748038, 5203832230045378, 
                5164860603923613, 5197557698575370, 
                5204240503738299, 5362977367866992, 
                5509006394433883, 5307347394793965]
    in printf "Validated Incorrect MasterCard test %d / %d" (length (filter (==False) [isMaster x | x <- nums])) (length nums)

testVisaCorrect :: String
testVisaCorrect =
    let nums = [4485671656947391, 4916414782911976, 
                4532207748348158, 4959666344196184, 
                4716065903358810, 4485975018489202, 
                4716636136055322, 4929403223670452, 
                4929886258966250, 4485295175387812]
    in printf "Validated Correct Visa test %d / %d" (length (filter (==True) [isVisa x | x <- nums])) (length nums)
    

testVisaWrong :: String
testVisaWrong =
    let nums = [4485671656947390, 4916414782911975, 
                4532207748348157, 4959666344196183, 
                4716065903358819, 4485975018489201, 
                4716636136055321, 4929403223670451, 
                4929886258966259, 4485295175387811]
    in printf "Validated Incorrect Visa test %d / %d" (length (filter (==False) [isVisa x | x <- nums])) (length nums)

testAmericanCorrect :: String
testAmericanCorrect =
     let nums = [342045679384653, 344606141970896, 
                 343814242652717, 378344379264654, 
                 379352002963872, 348215618498203,
                 345218770356873, 340015371909224, 
                 375267441092261, 375805143520889]
    in printf "Validated Correct American Express test %d / %d" (length (filter (==True) [isAmericanExpress x | x <- nums])) (length nums)

testAmericanWrong :: String
testAmericanWrong =
     let nums = [342045679384652, 344606141870896, 
                 343814242552717, 378344379164654, 
                 379352002863872, 348215618488203,
                 345217770356873, 340015371909124, 
                 375267440092261, 375805143520789]
    in printf "Validated Incorrect American Express test %d / %d" (length (filter (==False) [isAmericanExpress x | x <- nums])) (length nums)

{- 
 * The functions above test our AmericanExpress, Visa and Mastercard validators.
 * We got the test data from a third party: http://www.getcreditcardnumbers.com/
 * We tested the output of our functions on both right and wrong credit cards.
 * In our limited sample size we found that our False Positives and False Negatives 
 * were both equal to zero while our True Positives and True negatives were equal 
 * to the length of our sample size (10 respectively).
 * This assumes that the third party testing data is actually correct because you 
 * can't get around this fact with the resources we have been provided with.
-}

{- 
 * Exercise 8
-}
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True 

accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)

accuses Arnold x = accuses Matthew x /= accuses Peter x 

accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers boy = [b | b <- boys, accuses b boy]

checkRequirements :: Boy -> [Boy] -> Bool
checkRequirements suspect liars = all (== True) [if x `elem` liars 
                                   then not (accuses x suspect) 
                                   else accuses x suspect 
                                   | x <- boys]

findCulprit :: (Bool, Boy, [Boy])
findCulprit = head (filter (\ (n, _, _) -> n) [(checkRequirements b x, b, x) | b <- boys, x <- listPermutations boys 2])

listPermutations :: [a] -> Integer -> [[a]]
listPermutations _ 0      = [[]]
listPermutations [] _     = []
listPermutations (x:xs) n = fmap (x:) (listPermutations xs (n - 1)) ++ listPermutations xs n

guilty :: [Boy]
guilty = [(\ (_, x, _) -> x) findCulprit]

honest :: [Boy]
honest = let
            xs = (\ (_, _, x) -> x) findCulprit
         in filter (`notElem` xs) boys

{- 
 * Bonus
 * Euler 9
 * Answer: 31875000
-}
euler9 :: Integer
euler9 =
    head [a*b*c | c <- [1..500], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c==1000]


{-
 * Bonus
 * Euler 10
 * Answer: 142913828922
-}
euler10 :: Integer
euler10 = sum (takeWhile (<2000000) primes)

{-
 * Bonus
 * Euler 49
 * Answer: 296962999629
-}
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements x y = null (x \\ y) && null (y \\ x)

checkSequence :: Integer -> [Integer]
checkSequence n = [a | a <- [1000..9999], (sameElements (digits a) (digits (a+n))), (sameElements (digits (a+n)) (digits (a+(2*n)))), prime a, prime (a+n), prime (a+2*n)]

euler49 :: String
euler49 = 
    let 
        start = checkSequence 3330 !! 1
        snd = (start + 3330)
    in show start ++ show snd ++ show (snd+3330)