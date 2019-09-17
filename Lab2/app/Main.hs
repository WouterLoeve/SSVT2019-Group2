import Lab2solutions
import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char

main = do
    printf "== Assignment 1 ==\n"
    print "Testing uniformity of generated numbers in quartiles with multiple Chi Square tests:"
    chiSquareTest 1000
    chiSquareTest 1000
    chiSquareTest 1000
    chiSquareTest 10000
    chiSquareTest 10000
    chiSquareTest 10000
    printf "== Assignment 2 ==\n"
    print "Testing properties of triangles"
    putStr testTriangle
    printf "== Assignment 3 ==\n"
    print "Testing sorting properties"
    print testSortProperties
    printf "== Assignment 4 ==\n"
    print "Testing same whether permutations always have the same lengths"
    quickCheck prop_sameListLengthPerm
    print "Testing same whether permutations always have same nummer of elements"
    quickCheck prop_numOccurencePerm
    printf "== Assignment 5 ==\n"
    print "derangement test same objects"
    quickCheck prop_sameObjectsDeran
    print "derangement test same list length"
    quickCheck prop_sameListLengthDeran
    print "derangement test number of occurence"
    quickCheck prop_numOccurenceDeran
    print "derangement test same position"
    quickCheck prop_samePosDeran
    print "derangement test never different from input list"
    quickCheck prop_unequalDeran
    --
    printf "== Assignment 6 ==\n"
    testRot13Props

    printf "== Assignment 7 ==\nTesting IBAN\n"
    putStr testIban
