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
    let prepend = if numFail > 0 then "**FAIL**  " else "  PASS    "
    let append = if numCases == numPass then "" else " ("++ show numFail ++" Failed)"
    prepend ++ testName ++ " : " ++ show numPass ++ " / " ++ show numCases ++ " passed" ++ append

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


-- contradiction & tautology
testCaseContradiction = Equiv p (Neg p)     -- p == !p
testCaseTautology = Equiv p p               -- p == p

{-
 - formEntailsA (just p) implies formEntailsB (always True)
 - formEntailsB does not imply formEntailsA 
 -     (there is a case where f1=True but f2=False for same params)
 -}
testCaseEntailsA = p                        -- p
testCaseEntailsB = Dsj [p, Neg p]         -- p or !p

-- Test using demorgan's theorem
testCaseEquivA = Neg (Dsj [p, q])           -- !(p or q)
testCaseEquivB = Cnj [Neg p, Neg q]     -- (!p and !q)

testProperties = 
    contradiction testCaseContradiction &&
    tautology testCaseContradiction &&
    testCaseEntailsA `entails` testCaseEntailsB &&
    not (testCaseEntailsB `entails`testCaseEntailsA) &&
    testCaseEquivA `equiv` testCaseEquivB

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

prop_equivParse :: Form -> Property
prop_equivParse f = True ==> equiv (showParse f) f

prop_tautParse :: Form -> Property
prop_tautParse f = True ==> tautology f == tautology (showParse f)

prop_contraParse :: Form -> Property
prop_contraParse f = True ==> contradiction f == contradiction (showParse f)

prop_rEntailsParse :: Form -> Property
prop_rEntailsParse f = True ==> f `entails` showParse f

prop_lEntailsParse :: Form -> Property
prop_lEntailsParse f = True ==> showParse f `entails` f

-- Not sure about this one??????
prop_testShow :: Form -> Property
prop_testShow f = True ==> show (showParse f) == show f

-- More testing methods???
-- Unit tests:
-- Explain + Use them on the show function and thend
-- explain that we can then use show in our quickcheck tests.
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
    
testParseKnownCases = do
    let numCases = length parseKnownCases
    let numPass = length (filter (==True) [parse a == [b] | (a,b) <- parseKnownCases])
    testRunHelper "known cases" numCases numPass

testParse :: IO ()
testParse = do
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
    print "Testing known correct cases"
    let numCases = length parseKnownCases
    let numPass = length (filter (==True) [parse a == [b] | (a,b) <- parseKnownCases])
    print (testRunHelper "known cases" numCases numPass)

{- 
 - Exercise 3
 - Time: 120 min
 - A: 
 - Testing: 
    - equivalence with f and CNF f
    - if f is a tautology, CNF f should also be
    - Same goes for contradiction
-}
valToForm :: (Name, Bool) -> Form
valToForm (name, val)
    | val = Neg $ Prop name
    | otherwise = Prop name

rowToClause :: Valuation -> Form
rowToClause row 
    | length row == 1 = valToForm $ head row
    | otherwise = Dsj $ valToForm <$> row

toCNF :: Form -> Form
toCNF f 
    | length falseRows == 1 = rowToClause $ head falseRows 
    | otherwise =  Cnj $ rowToClause <$> falseRows
    where falseRows = filter (\ v -> not $ evl v f) (allVals f)

{- 
 - Exercise 4
 - Time: 140 min
-}

{- 
Test Report:
- Tests the Conjunctive Normal Form converter from exercise 3.
- The following properties have been defined:
    - Equivalence between the original form and the one converted to the CNF
    - Retension of tautology in original form and converted.
    - Retension of contradiction in original form and converted.
    - Right entails; original entails converted
    - Left entailss; converted entails original
    - Check whether CNF is actually in CNF format.
- To test these properties a generator for forms was written, 
    which quikcheck can utilise.
- This recursive generator is bounded by a sized paramater 
    to guarantee it does not loop infinetely.
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

isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False

isClause :: Form -> Bool
isClause (Dsj ls) = all isLiteral ls
isClause f = isLiteral f

isCNF :: Form -> Bool
isCNF (Cnj cs) = all isClause cs
isCNF f = isClause f

prop_followsGrammar :: Form -> Bool
prop_followsGrammar f = isCNF $ toCNF f

arbForm :: Integral a => a -> Gen Form
arbForm 0 = fmap Prop (suchThat arbitrary (>0))
arbForm n = frequency
    [ (1, fmap      Prop (suchThat arbitrary (>0))),
      (1, fmap      Neg param),
      (1, liftM2    Impl param param),
      (1, liftM2    Equiv param param),
      (1, fmap      Dsj (vectorOf 2 param)),
      (1, fmap      Cnj (vectorOf 2 param)) ]
    where param = arbForm (n `div` 2)

instance Arbitrary Form where
    arbitrary = sized arbForm
  
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
 - Property 2: Test whether all subtrees are accounted for
-}


sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

prop_isSubSetSub :: Form -> Property
prop_isSubSetSub f = True ==> all (==True) $ map (\x -> show x `isInfixOf` fStr ) ((\ (Set l) -> l) (sub f))
    where fStr = show f

testSub :: IO ()
testSub = do
    print "Tests whether the subtrees of f are actually a subset of f"
    quickCheck prop_isSubSetSub
    print ""
    -- quickCheck (forAll genForms prop_isSubSetSub)

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
-}

type Clause = [Int]
type Clauses = [Clause]

literal2int :: Form -> Int
literal2int (Prop name) = name
literal2int (Neg (Prop name)) = - name

clause2cl :: Form -> Clause
clause2cl (Dsj ls) = literal2int <$> ls
clause2cl f = [literal2int f]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj cs) = clause2cl <$> cs
cnf2cls f = [clause2cl f]

{- 
Testing Ideas:
For each function/disjunction do:
- Check whether variables are all accounted for
- Check length of the list
- Check negation

- Make individual generators designed for each function.(stripped down version of our form generator)
-}
