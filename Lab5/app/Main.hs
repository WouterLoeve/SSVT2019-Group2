import Data.List
import Test.QuickCheck
import Data.Char
import Lab5solutions
import Lecture5
import Benchmark

main :: IO ()
main = do
    print "Please run this compiled to ensure that it is fast enough."
    print "--Exercise 1--"
    testExm
    benchExm
    print "--Exercise 2--"
    testComposites
    print "--Exercise 3--"
    leastComposites
    print "--Exercise 4--"
    leastComposites'
    print "--Exercise 5--"
    someMPrimes 12
    print "--Exercise 6--"
    testTrees
    print "--Exercise 7--"
    {- 
    - Testing with 1024 because 2048 takes too long for multiple runs.
    -}
    rsaTestMult 10 1024
