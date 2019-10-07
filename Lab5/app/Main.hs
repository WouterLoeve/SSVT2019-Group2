import Data.List
import Test.QuickCheck
import Data.Char
import Lab5solutions
import Lecture5
import Benchmark

main :: IO ()
-- main =  (primeMR 1 (2^128)) >>= print
main = do
    print "--Exercise 1--"
    -- testExm
    -- print "Testing Large number"
    -- large
    -- print "Testing small number"
    -- small
    -- print "Testing Insane number"
    -- largest
    print "--Exercise 2--"
    testComposites
    print "--Exercise 3--"
    -- leastComposites
    print "--Exercise 4--"
    -- leastComposites'
    print "--Exercise 5--"
    -- someMPrimes 8
    
    print "--Exercise 6--"

    print "--Exercise 7--"
    rsaTestMult 10 1024
