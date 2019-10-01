import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Lab4solutions

main :: IO ()
main = do
    print $ euler235 0 2
    print "Generator size is bounded but could still run for quite a while; we recommend building it instead of running it with the interactive mode."
    print "--Exercise 1--"
    testSetOwnGen
    testSetProp
    print "--Exercise 2--"
    testSetOperators
    testSetOperatorsOwnGen
    print "--Exercise (3 &) 4--"
    -- testCNF
    print "--Exercise 5--"
    -- testSub
    -- testNsub
    print "--Exercise 6--"
    -- testClause

