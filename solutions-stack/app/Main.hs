import Lab1solutions
import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Text.JSON.Generic

main = do
    printf "Assignments 2 and 3 can take a very long time for inputs and are thus commented out in Main.hs\n"
    printf "== Assignment 1.1 ==\n"
    quickCheck $ forAll genPositiveIntegers testSumSquares
    
    printf "== Assignment 1.2 ==\n"
    quickCheck $ forAll genPositiveIntegers testSumCubes

    printf "== Assignment 2 ==\n"
    -- quickCheck $ forAll genPositiveIntegers testPowerCardinality
    
    printf "== Assignment 3 ==\n"
    -- quickCheck $ forAll genPositiveIntegers testPermsCardinality
    
    printf "== Assignment 4 ==\n"
    printf "First 10 reversible primes: %s\n" (show (take 10 reversiblePrimes))

    printf "== Assignment 5 ==\n"
    printf "Smallest consecutive prime: %d\n" smallestConsecutivePrime

    printf "== Assignment 6 ==\n"
    printf "smallest counter example: %s\n" (show smallestCounterExample)

    printf "== Assignment 7 ==\n"
    printf "%s\n" testMastercardCorrect
    printf "%s\n" testMastercardWrong
    printf "%s\n" testVisaCorrect
    printf "%s\n" testVisaWrong
    printf "%s\n" testAmericanWrong
    printf "%s\n" testAmericanCorrect

    printf "== Assignment 8 ==\n"
    printf "Guilty: %s\n" (show guilty)
    printf "Honest: %s\n" (show honest)

    printf "== Bonus 1 ==\n"
    printf "%d\n" euler9 

    printf "== Bonus 2 ==\n"
    printf "%d\n" euler10

    printf "== Bonus 3 ==\n"
    printf "%s\n" euler49