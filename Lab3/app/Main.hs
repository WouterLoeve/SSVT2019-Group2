import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Lab3solutions

main :: IO ()
main = do
    testProperties
    testParse
    testCNF
    testSub
    testNsub
    testClause
