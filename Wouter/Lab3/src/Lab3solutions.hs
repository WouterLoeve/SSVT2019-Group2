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
 - Exercise 1
 - Time: 30 min
 - A: 
-}
getTruthTable :: Form -> [Bool]
getTruthTable f = map (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (==False) (getTruthTable f)

tautology :: Form -> Bool
tautology f = all (==True) (getTruthTable f)

-- | logical entailment 
-- foreach assignment, form1 is true, form2 should also be true
entails :: Form -> Form -> Bool
entails f1 f2 = tautology $ Impl f2 f1

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1

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
-- Unit tests?

testParse :: IO ()
testParse = do
    print "Testing equivalence between parsed version and normal version:"
    quickCheck (forAll genForms prop_equivParse)
    print "Testing tautology between parsed version and normal version:"
    quickCheck (forAll genForms prop_tautParse)
    print "Testing contradiction between parsed version and normal version:"
    quickCheck (forAll genForms prop_contraParse)
    print "Testing right entails between parsed version and normal version:"
    quickCheck (forAll genForms prop_rEntailsParse)
    print "Testing left entails between parsed version and normal version:"
    quickCheck (forAll genForms prop_lEntailsParse)
    print "Testing whether conversion is in parsed form:"
    quickCheck (forAll genForms prop_followsGrammar)
    print "Testing subsequent usage of show and parse"
    quickCheck (forAll genForms prop_testShow)

{- 
 - Exercise 3
 - Time: 120 min
 - A: 
 - Testing: 
    - equivalence with f and CNF f
    - if f is a tautology, CNF f should also be
    - Same goes for contradiction
-}
converToCNF :: Form -> Form
converToCNF f | tautology f = Cnj [Dsj [Prop 1, (Neg . Prop) 1], Dsj [Prop 1, (Neg . Prop) 1]]
              | contradiction f = Cnj [Prop 1, (Neg . Prop) 1]
              | otherwise = Cnj arr
            where arr = [ convertStmt x | x <- allVals f, not $ evl x f]

convertStmt :: [(Name, Bool)] -> Form
convertStmt x = Dsj $ convertSingle <$> x

convertSingle :: (Name, Bool) -> Form
convertSingle (key, val) | not val = Prop key
                         | val = (Neg . Prop) key

{- 
 - Exercise 4
 - Time: 140 min
 - A: 
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
prop_equivCNF f = True ==> equiv (converToCNF f) f

prop_tautCNF :: Form -> Property
prop_tautCNF f = True ==> tautology f == tautology (converToCNF f)

prop_contraCNF :: Form -> Property
prop_contraCNF f = True ==> contradiction f == contradiction (converToCNF f)

prop_rEntailsCNF:: Form -> Property
prop_rEntailsCNF f = True ==> f `entails` converToCNF f

prop_lEntailsCNF:: Form -> Property
prop_lEntailsCNF f = True ==> converToCNF f `entails` f

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
prop_followsGrammar f = isCNF $ converToCNF f

genForms :: Gen Form
genForms = sized $ \ n -> genForms' (round (sqrt (fromIntegral n)))

genForms' :: Integer -> Gen Form
genForms' 0 = fmap Prop (suchThat arbitrary (>0))
genForms' n = oneof [fmap Prop (suchThat arbitrary (\ n -> n > 0 && n < 5)),
                       fmap Neg poss1,
                       liftM2 Impl poss1 poss2,
                       liftM2 Equiv poss1 poss2,
                       fmap Dsj (vectorOf 2 poss1),
                       fmap Cnj (vectorOf 2 poss1)]
                where 
                    newN = n `div` 2
                    poss1 = genForms' newN
                    poss2 = genForms' (newN `div` 2)
  
testCNF :: IO ()
testCNF = do
    print "Testing equivalence between CNF version and normal version:"
    quickCheck (forAll genForms prop_equivCNF)
    print "Testing tautology between CNF version and normal version:"
    quickCheck (forAll genForms prop_tautCNF)
    print "Testing contradiction between CNF version and normal version:"
    quickCheck (forAll genForms prop_contraCNF)
    print "Testing right entails between CNF version and normal version:"
    quickCheck (forAll genForms prop_rEntailsCNF)
    print "Testing left entails between CNF version and normal version:"
    quickCheck (forAll genForms prop_lEntailsCNF)
    print "Testing whether conversion is in CNF form:"
    quickCheck (forAll genForms prop_followsGrammar)


{- 
 - Exercise 5
 - Time: 30 min
 - Property 1: Tests whether the subtrees of f are actually a subset of f.
 - Property 2: 
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


    -- prop_isMissingSub f = True ==> map (\x -> strReplace (show x) "" fStr ) ((\ (Set l) -> l) (sub f))
    -- where fStr = show fstrReplace

replace :: String -> String -> String -> String
replace subStr repl str = T.unpack $ T.replace subStrT replT strT
    where 
        subStrT = T.pack subStr
        replT = T.pack repl
        strT = T.pack str

-- prop_isMissingSub :: Form -> Property
-- prop_isMissingSub f = map (\x -> replace (show x) "" fStr ) ((\ (Set l) -> l) (sub f))
--     where fStr = show f

testSub :: IO ()
testSub = do
    print "Tests whether the subtrees of f are actually a subset of f"
    quickCheck (forAll genForms prop_isSubSetSub)
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