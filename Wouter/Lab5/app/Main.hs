import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Lab4solutions

main :: IO ()
main = do
    -- print $ euler235 0 2
    print "Generator size is bounded but could still run for quite a while; we recommend building it instead of running it with the interactive mode."
    print "--Exercise 1--"
    testSetOwnGen
    testSetProp
    print "--Exercise 2--"
    testSetOperators
    testSetOperatorsOwnGen
    print "--Exercise (3 &) 4--"
    testSetOperators
    testSetOperatorsOwnGen
    testIsSerial
    print "--Exercise 6--"
    testSymClos
    testTrClos
    testTrClos2
    print "--Exercise 7--"
    print "(See implementation for reasoning)"
    illustrateDiff
    print "--Exercise 8--"

    print "--Exercise 9--"
