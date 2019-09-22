module Lab3solutions where
import Data.List
import Data.Char
import Data.Tuple
import SetOrd
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import Lecture3

{-
 - evlHelper takes a function and runs all combinations of input parameters, 
 - returning a list of results
 -}
evlHelper :: Form -> [Bool]
evlHelper f = (map (\ v -> evl v f) (allVals f))

{-
 - evlHelper' taks two functions and runs all combinations of input parameters,
 - determined by the first function. Returning a list of tuples, results of f1 and f2
 - TODO: sort by propNames size, biggest first
 -}
evlHelper' :: Form -> Form -> [(Bool, Bool)]
evlHelper' f1 f2 = (map (\ v -> ((evl v f1), (evl v f2))) (allVals f1))

{-
 - testRunhelper helps print results
 -}
testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "**FAIL**  " else "  PASS    "
    let append = if numCases == numPass then "" else " ("++(show numFail)++" Failed)"
    prepend ++ testName ++ " : " ++ (show numPass) ++ " / " ++ (show numCases) ++ " passed" ++ append

{- 
 - Exercise 1
 - Time: 30 min
 - A:
 - TODO
 -}
-- Definition of properties
contradiction :: Form -> Bool
tautology :: Form -> Bool
entails :: Form -> Form -> Bool
equiv :: Form -> Form -> Bool

-- Implementation of properties
contradiction f = all (==False) (evlHelper f)
tautology f = all (==True) (evlHelper f) 
entails f1 f2 = all (==True) (map (\ (a,b) -> (not a || (a && b))) (evlHelper' f1 f2))
equiv f1 f2 = all (==True) (map (\ (a,b) -> a == b) (evlHelper' f1 f2))

-- Testing of properties

-- contradiction & tautology
test_contradiction = Equiv p (Neg p)     -- p == !p
test_tautology = Equiv p p               -- p == p

{-
 - formEntailsF1 (just p) implies formEntailsF2 (always True)
 - formEntailsF2 does not imply formEntailsF2 
 -     (there is a case where f1=True but f2=False for same params)
 -}
test_entailsA = p                        -- p
test_entailsB = Dsj [p, (Neg p)]         -- p or !p

-- Test using demorgan's theorem
test_equivA = Neg (Dsj [p, q])           -- !(p or q)
test_equivB = Cnj [(Neg p), (Neg q)]     -- (!p and !q)

testProperties = 
    contradiction (test_contradiction) &&
    tautology test_tautology &&
    entails test_entailsA test_entailsB &&
    equiv test_equivA test_equivB

{- 
 - Exercise 2
 - Time: 60 min
 - A:
 - To test the parse implementation, I opted to create a number of strings and their expected outcomes
 - My initial idea was to be able to use strings with placeholder that during testing substrings could be
 - inserted into. This would effectively test nested expressions. Here I ran into trouble with defining the expected
 - output, as generating statements such as (Dsj [p, q, (Equiv p q)]) is much more difficult than doing string
 - substitution. Nevertheless, this might still be a viable strategy to test if the parser can handle nested expressions
 - well but it would be unclear if the answer is correct. (apart from crashes or unrecognized tokens as comparison)
 -} 

testParse_cases =
    ("1",                 (p)):
    ("2",                 (q)):
    ("3",                 (r)):
    ("-1",                (Neg p)):
    ("--1",               (Neg (Neg p))):
    ("(1<=>2)",           (Equiv p q)):
    ("(1==>2)",           (Impl p q)):
    ("+(1 2 3)",          (Dsj [p, q, r])):
    ("*(1 2 3)",          (Cnj [p, q, r])):
    []

{-
 - the parse symmetry property states that when a Form is represented as a string, and this string is fed into the parse
 - function. the parse function should return the origignal Form object.
 -}
prop_parseSymmetry :: Form -> Bool
prop_parseSymmetry f = f == head (parse (show f))

testParseCases = do
    let numCases = length testParse_cases
    let numPass = length (filter (\v -> v) [parse a == [b] | (a,b) <- testParse_cases])
    testRunHelper "Parse:known cases" numCases numPass

testParseSymmetry = do
    putStrLn "Parse:symmetry property"
    quickCheck prop_parseSymmetry

{- 
 - Exercise 3
 - Time: 120 min
 - A:
 - In order to implement the cnf function, I decided to first convert the equation to nnf.
 - This was convenient, as the provided Lecture code already implemented a function to convert
 - arbitrary equations into nnf format. After the conversion, I apply the distributive laws to
 - remove Conjunctions from the formula. 
 -}
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree
    where
      cnf' :: Form -> Form
      cnf' (Cnj fs)    = Cnj (map cnf' fs)
      cnf' (Dsj fs)    = dist (map cnf' fs)
      cnf' f           = f
      
      -- dist recieves a list of the original Dsj arguemnts.
      dist :: [Form] -> Form
      dist [Cnj [a, b], c] = Cnj [(dist [a, c]), (dist [b, c])]
      dist [a, Cnj [b, c]] = Cnj [(dist [a, b]), (dist [a, c])]
      dist fs | length fs > 3 = dist [head fs, dist (tail fs)]
              | otherwise     = Dsj fs

prop_cnfSymmetry :: Form -> Bool
prop_cnfSymmetry f =
    evlHelper f == evlHelper (cnf f)

testCnfCases = do
    let numCases = length testParse_cases
    let numPass = length (filter (\v -> v) [(evlHelper (cnf b)) == (evlHelper b) | (a,b) <- testParse_cases])
    testRunHelper "cnf:output comparison" numCases numPass

testCnfSymmetry = do
    verboseCheck prop_cnfSymmetry

{- 
 - Exercise 4
 - Time: 90 min
 - A: 
 - By defining the behaviour Arbitrary for the type Form, we can tell QuickCheck how it should
 - generate a random Form. In this imlplementation, I used frequency so it is easy to tweak
 - the generator function to produce desired equations. by altering the frequency we can for example
 - increase the likelihood of short, single atom equations or the opposite.
 -} 

-- generate arbitrary :: IO Form
arbForm 0 = liftM Prop (suchThat arbitrary (>0))
arbForm n = frequency
    [ (1, liftM     Prop (suchThat arbitrary (>0))),
      (1, liftM     Neg param),
      (1, liftM2    Impl param param),
      (1, liftM2    Equiv param param),
      (1, fmap      Dsj (vectorOf 2 param)),
      (1, fmap      Cnj (vectorOf 2 param)) ]
     where param = (arbForm (n `div` 2))

instance Arbitrary Form where
    arbitrary = sized arbForm
    
{- 
 - Exercise 5
 - Time:  min
 - A:
 - property 1: The size of the subtree must be smaller than that of the parent
 - property 2: 
 -} 

-- prop 1: sub returns at least 1?
type Name = Int

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

