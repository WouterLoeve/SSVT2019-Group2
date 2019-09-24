module Lab3solutions where
import Data.List
import Data.Char
import Data.Tuple
import qualified Data.Text as T
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import SetOrd
import Lecture3

{-
 -
 -}
{-
 - testRunhelper helps print results
 -}
testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "--- FAIL" else "+++ OK"
    let append = if numCases == numPass then "" else " ("++ show numFail ++" Failed)"
    prepend ++ ", Test '" ++ testName ++ "' Passed " ++ show numPass ++ " out of " ++ show numCases ++ " cases" ++ append

{-
 - Exercise 1
 - Time: 30 min
 - A:
-}
contradiction, tautology :: Form -> Bool
contradiction = not . satisfiable
tautology f = all (`evl` f) (allVals f)

entails, equiv :: Form -> Form -> Bool
f `entails` g = tautology $ Impl g f
f `equiv` g = tautology $ Equiv f g


testContradictions, testTautologies :: [Form] -> Bool
testContradictions fs = all (==True) $ contradiction <$> fs
testTautologies fs = all (==True) $ tautology <$> fs

testEntails, testEquivs :: [Form] -> [Form] -> Bool
f `testEntails` g = all (==True) $ zipWith entails f g
f `testEquivs` g = all (==True) $ zipWith equiv f g

{-
 - Each logical form in testCaseContradiction should return false for
 - every input of the variables.
 -
 - For three different cases, the contradiction function is tested by passing
 - the list of forms to testContradictions.
 -}
testCaseContradiction = [Equiv p (Neg p),
                         Cnj [Equiv p (Neg q), Equiv q (Neg q)],
                         Dsj [Equiv p (Neg p), Equiv q (Neg q)]]

{-
 - Each logical form in testCaseTautology should return true for
 - every input of the variables.
 -
 - For three different cases, the tautology function is tested by passing
 - the list of forms to testTautologies.
 -}

testCaseTautology = [Equiv p p,
                     Dsj [p, Neg p],
                     Equiv (Neg (Cnj [p, q])) (Dsj [Neg p, Neg q])]

{-
 - testCaseEntailsB implies testCaseEntailsA
 - testCaseEntailsA does not imply testCaseEntailsB
 -     (there is a case where f1=True but f2=False for same params)
 -
 - For three different cases, the entails function is tested by passing the
 - two lists to testEntails.
 -}
testCaseEntailsA = [p,
                    Dsj [q, Dsj [p, Neg p]],
                    Dsj [p, r]]
testCaseEntailsB = [Dsj [p, Neg p],
                    Dsj [p, Neg p],
                    Dsj [p, Dsj [q, r]]]

{-
 - testCaseEquivA should have the same logical content as testCaseEquivB

 - For three different cases, the equiv function is tested by passing the
 - two lists to testEquivs.
 -}

testCaseEquivA = [Neg (Dsj [p, q]),
                  Dsj [p, p],
                  Cnj [p, Dsj [p, q]]]
testCaseEquivB = [Cnj [Neg p, Neg q],
                  Cnj [p, p],
                  Dsj [p, Cnj [p, q]]]
{-
 - Shortcut function to test all of the logical functions in one go.
 -}

{-
 - We store the testcases in an array to print the pass/fail ratio.
 -}
propertiesTestCases = [
    (testContradictions testCaseContradiction),
    (testTautologies testCaseTautology),
    (testCaseEntailsB `testEntails` testCaseEntailsA),
    (not (testCaseEntailsA `testEntails`testCaseEntailsB)),
    (testCaseEquivA `testEquivs` testCaseEquivB)]

testProperties = do
    putStrLn (testRunHelper "testProperties" (length propertiesTestCases) (length (filter (==True) propertiesTestCases)))

{-
 - Exercise 2
 - Time: 10 min
 - Options:
    - use Format's to convert to String with Show, Parse them and use equiv.
    - make seperate Format's and String notations and then use equiv
    - Same as the above but then compare strings
    - Instead of equiv check if contradiction, entails and tautology yield the same results
-}
showParse :: Form -> Form
showParse f = head $ parse $ show f

-- Property 1, an equation is equivalent to the paresed version of itself
prop_equivParse :: Form -> Property
prop_equivParse f = True ==> equiv (showParse f) f

-- Property 2, an equation that is a tautology is still a tautology after parsing
prop_tautParse :: Form -> Property
prop_tautParse f = True ==> tautology f == tautology (showParse f)

-- Property 3, an equation that is a contradiction is still a contradiction after parsing
prop_contraParse :: Form -> Property
prop_contraParse f = True ==> contradiction f == contradiction (showParse f)

-- Property 4, an equation entails it's parsed form
prop_rEntailsParse :: Form -> Property
prop_rEntailsParse f = True ==> f `entails` showParse f

-- Property 5, a parsed equation entails the original equation
prop_lEntailsParse :: Form -> Property
prop_lEntailsParse f = True ==> showParse f `entails` f

-- Property 6, an the representation of an equation does not change after parsing
prop_testShow :: Form -> Property
prop_testShow f = True ==> show (showParse f) == show f

{-
 - We define a list of known equations and their respective string representation.
 - We can then use this list to test both the parse function (string to equation)
 - As well as the implementation of `show` for the type Form (equation to string).
 - Testing of the `show` method is included because the above properties
 - to test the parse method depend on a correct implementation of `show` for
 - the type Form. We chose to use predefined strings and equations for this test,
 - because in order to find bugs in the `show` implementation, we need references
 - of correct strings and equations
 -}
parseKnownCases = [
    ("1",        p),
    ("2",        q),
    ("3",        r),
    ("-1",       Neg p),
    ("--1",      Neg (Neg p)),
    ("(1<=>2)",  Equiv p q),
    ("(1==>2)",  Impl p q),
    ("+(1 2 3)", Dsj [p, q, r]),
    ("*(1 2 3)", Cnj [p, q, r])]

testParse :: IO ()
testParse = do
    -- QuickCheck tests
    print "Testing equivalence between parsed version and normal version:"
    quickCheck prop_equivParse
    print "Testing tautology between parsed version and normal version:"
    quickCheck prop_tautParse
    print "Testing contradiction between parsed version and normal version:"
    quickCheck prop_contraParse
    print "Testing right entails between parsed version and normal version:"
    quickCheck prop_rEntailsParse
    print "Testing left entails between parsed version and normal version:"
    quickCheck prop_lEntailsParse
    print "Testing whether conversion is in parsed form:"
    quickCheck prop_followsGrammar
    print "Testing subsequent usage of show and parse"
    quickCheck prop_testShow

    -- test if parse on a string produces the expected equation
    print "Testing parsing of known strings"
    let numCases = length parseKnownCases
    let numPassStrings = length (filter (==True) [parse a == [b] | (a,b) <- parseKnownCases])
    putStrLn (testRunHelper "known show to equations" numCases numPassStrings)

    -- test if show on an equation produces expected string
    print "Testing show of known equations"
    let numPassEquations = length (filter (==True) [show b == a | (a,b) <- parseKnownCases])
    putStrLn (testRunHelper "known equations to show" numCases numPassEquations)

{-
 - Exercise 3
 - Time: 120 min
-}


{-
 - Inverts the inidividual atoms.
 -}
valToForm :: (Name, Bool) -> Form
valToForm (name, val)
    | val = Neg $ Prop name
    | otherwise = Prop name

{-
 - Converts a row to a clause used in the toCNF function.
 -}
rowToClause :: Valuation -> Form
rowToClause row
    | length row == 1 = valToForm $ head row
    | otherwise = Dsj $ valToForm <$> row

{-
 - Converts a form to its Conjunctive normal form counterpart.
 - The top level of this form is a conjunction which in turn consists of disjunctions.
 - The construction is done by taking all rows in the truth table which correspond to false.
 - The atoms are inverted and put in a disjunction for each row. These are then conjuncted to create the CNF form.
 -}
toCNF :: Form -> Form
toCNF f
    | length falseRows == 1 = rowToClause $ head falseRows
    | otherwise =  Cnj $ rowToClause <$> falseRows
    where falseRows = filter (\ v -> not $ evl v f) (allVals f)

{-
 - Exercise 4
 - Time: 140 min

Test Report:
- Tests the Conjunctive Normal Form converter from exercise 3.
- The following properties have been defined:
    - Equivalence between the original form and the one converted to the CNF
    - Retension of tautology in original form and converted.
    - Retension of contradiction in original form and converted.
    - Right entails; original entails converted
    - Left entailss; converted entails original
    - Check whether CNF is in CNF format.
-}

prop_equivCNF :: Form -> Property
prop_equivCNF f = True ==> equiv (toCNF f) f

prop_tautCNF :: Form -> Property
prop_tautCNF f = True ==> tautology f == tautology (toCNF f)

prop_contraCNF :: Form -> Property
prop_contraCNF f = True ==> contradiction f == contradiction (toCNF f)

prop_rEntailsCNF:: Form -> Property
prop_rEntailsCNF f = True ==> f `entails` toCNF f

prop_lEntailsCNF:: Form -> Property
prop_lEntailsCNF f = True ==> toCNF f `entails` f


{-
 - Checks whether a form is a literal.
 - Used in the prop_followsGrammar process.
-}
isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False


{-
 - Checks whether a form is in clause format
 - Used in the prop_followsGrammar process.
-}
isClause :: Form -> Bool
isClause (Dsj ls) = all isLiteral ls
isClause f = isLiteral f

{-
 - Checks whether the subsets of the conjunctions are all clauses.
 - Used in the prop_followsGrammar function.
-}
isCNF :: Form -> Bool
isCNF (Cnj cs) = all isClause cs
isCNF f = isClause f

{-
 - Checks whether a Form follows the CNF grammar.
-}
prop_followsGrammar :: Form -> Bool
prop_followsGrammar f = isCNF $ toCNF f

{-
To test these properties a generator (arbForm) for forms was written,
    which quikcheck can utilise.
This recursive generator is bounded by a sized paramater
    to guarantee it does not loop infinetely.
We then take the square root of the of the size the sized gives us.
We do this since the CNF function doesn't scale favourably in quickChecks aggresive size upscaling.
Frequencies have also been defined to allow for easy frequency
    changing based on the application of the boolean functions.
We do not advise to run this function trough the interpreter as it's significantly slower compared to building it and executing it.
The instruction to do so are in the readme of the git repository.
-}

arbForm' :: Integral a => a -> Gen Form
arbForm' 0 = fmap Prop (suchThat arbitrary (>0))
arbForm' n = frequency
    [ (1, fmap      Prop (suchThat arbitrary (>0))),
      (1, fmap      Neg param),
      (1, liftM2    Impl param param),
      (1, liftM2    Equiv param param),
      (1, fmap      Dsj (vectorOf 2 param)),
      (1, fmap      Cnj (vectorOf 2 param)) ]
    where param = arbForm' (n `div` 2)

instance Arbitrary Form where
    arbitrary = sized $ \ n -> arbForm' (round (sqrt (fromIntegral n)))

testCNF :: IO ()
testCNF = do
    print "Testing equivalence between CNF version and normal version:"
    quickCheck prop_equivCNF
    print "Testing tautology between CNF version and normal version:"
    quickCheck prop_tautCNF
    print "Testing contradiction between CNF version and normal version:"
    quickCheck prop_contraCNF
    print "Testing right entails between CNF version and normal version:"
    quickCheck prop_rEntailsCNF
    print "Testing left entails between CNF version and normal version:"
    quickCheck prop_lEntailsCNF
    print "Testing whether conversion is in CNF form:"
    quickCheck prop_followsGrammar


{-
 - Exercise 5
 - Time: 30 min
 - Property 1: Tests whether the subtrees of f are actually a subset of f.
 - Property 2: Test whether the longest subtree is the tree itself
-}
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


{-
 - The first property tests whether the subtrees of f are actually a subset of f.
 - This is implemented by getting all of the sub-formulae of the given form.
 - For every sub-formula, the string is checked whether this is inside of the
 - string of the form. If all of the strings of the sub-formulae are inside
 - of the form, then the result will be True.
-}
prop_isSubSetSub :: Form -> Property
prop_isSubSetSub f = True ==> all (==True) $ map (\x -> show x `isInfixOf` fStr ) ((\ (Set l) -> l) (sub f))
    where fStr = show f

prop_longestSubtree :: Form -> Property
prop_longestSubtree f = True ==> (show f) == longest
    where longest = maximumBy (\a b -> compare (length a) (length b)) (show <$> (\ (Set l) -> l) (sub f))

testSub :: IO ()
testSub = do
    print "Tests whether the subtrees of f are actually a subset of f"
    quickCheck prop_isSubSetSub
    print "Tests whether longest subtree of f is f"
    quickCheck prop_longestSubtree

{-
 - The nsub function calculates the amount of sub-formulae of a given form, recursively.
 - To do this, the original sub function is used in order to recursively collect all
 - of the sub-formulae and the total amount of sub-formulae is then returned.
-}
nsub :: Form -> Int
nsub f = length $ (\ (Set l) -> l) (nsub' f)

nsub' :: Form -> Set Form
nsub' (Prop x) = Set [Prop x]
nsub' (Neg f) = unionSet (Set [Neg f]) (sub f)
nsub' f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
nsub' f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
nsub' f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
nsub' f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


{-
    Keep list and use union at the end.
-}

{-
 - Exercise 6
 - Time: 30 min
 - Testing: Test correct Int output and lengths of lists.
 - If the above properties hold, the whole should be correct as well.
 - It is difficult to test for correct contents without just reimplementing the conversion function.
 - In fact, prop_correctInt more or less already does this.
 - Additionally, test whether all variables are accounted for by checking substring of show.
 - To do this we create a generator of each part of the CNF.
 - Precondition for these properties is that the Form is in CNF.
-}

type Clause = [Int]
type Clauses = [Clause]

literal2int :: Form -> Int
literal2int (Prop name) = name
literal2int (Neg (Prop name)) = -name

clause2cl :: Form -> Clause
clause2cl (Dsj ls) = literal2int <$> ls
clause2cl f = [literal2int f]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj cs) = clause2cl <$> cs
cnf2cls f = [clause2cl f]


prop_correctInt :: Form -> Property
prop_correctInt f@(Prop name) = isLiteral f ==> name == literal2int f
prop_correctInt f@(Neg (Prop name)) = isLiteral f ==> -name == literal2int f

prop_correctClauseLength :: Form -> Property
prop_correctClauseLength f@(Dsj ls) = isClause f ==> length ls == (length . clause2cl) f
prop_correctClauseLength f = isClause f ==> (length . clause2cl) f == 1

prop_correctClausesLength :: Form -> Property
prop_correctClausesLength f@(Cnj cs) = isCNF f ==> length cs == (length . cnf2cls) f
prop_correctClausesLength f = isCNF f ==> (length . cnf2cls) f == 1

prop_allVariables :: Form -> Property
prop_allVariables f = isCNF f ==> all (`isInfixOf` show f) (show <$> (concat . cnf2cls) f)

gen_prop :: Gen Form
gen_prop = frequency
    [ (1, Prop <$> param),
      (1, Neg . Prop <$> param)]
    where param = suchThat arbitrary (>0)

gen_clause :: Gen Form
gen_clause = Dsj <$> listOf gen_prop

gen_CNF :: Gen Form
gen_CNF = Cnj <$> listOf gen_clause

testClause :: IO ()
testClause = do
    quickCheck $ forAll gen_prop prop_correctInt
    quickCheck $ forAll gen_clause prop_correctClauseLength
    quickCheck $ forAll gen_CNF prop_correctClausesLength
    quickCheck $ forAll gen_CNF prop_allVariables
