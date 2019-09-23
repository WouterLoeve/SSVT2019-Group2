import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Lab3solutions

main = do
    print "Testing properties"
    print (if testProperties then "PASS" else "FAIL" )
    testParse
    testCNF
    testSub
