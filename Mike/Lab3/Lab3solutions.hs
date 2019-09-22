module Lab3solutions where
import Lecture3
import Data.List
import Data.Char
import Data.Tuple
import SetOrd
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad

testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "**FAIL**  " else "  PASS    "
    let append = if numCases == numPass then "" else " ("++(show numFail)++" Failed)"
    prepend ++ testName ++ " : " ++ (show numPass) ++ " / " ++ (show numCases) ++ " passed" ++ append

{-
 - Exercise 1
 - Time: 25 min
 - A:
-}
-- contradiction :: Form -> Bool
-- contradiction f = all (\v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = all (\v -> evl v a) (filter (\v -> evl v b) (allVals b))

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = null (x \\ y) && null (y \\ x)
        where
            x = filter (\v -> evl v a) (allVals a)
            y = filter (\v -> evl v b) (allVals b)

{-
 - Exercise 1 Testing
 - Time: 60 min
-}

tautologyTest = Dsj [p, Neg p]

entailsTest1 = Dsj [p, q]
entailsTest2 = p

equivTest1 = Neg (Dsj [p, q])
equivTest2 = Cnj [Neg p, Neg q]


{-
 - Exercise 2
 - Time:
-}


{-
 - Exercise 3
 - Time: 70 min
-}

toConj :: Form -> Form -> Form
toConj (Cnj [f1, f2]) x = Cnj [(Dsj [f1, x]), (Dsj [f2, x])]
toConj x (Cnj [f3, f4]) = Cnj [(Dsj [x, f3]), (Dsj [x, f4])]
toConj x y = Dsj [x, y]

convertToCNF' :: Form -> Form
convertToCNF' (Cnj fs) = Cnj (map convertToCNF' fs)
convertToCNF' (Dsj [f1, f2]) = toConj (convertToCNF' f1) (convertToCNF' f2)
convertToCNF' f = f

convertToCNF :: [Form] -> Form
convertToCNF f = convertToCNF' $ nnf $ arrowfree (head f)

{-

-}

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

{-
 - Exercise 5
 - Time:
 - 1: Check for every sub formula, attrieved by `sub`, if it is a subset of the given formula.
-}

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

prop_allSubtrees :: Form -> Property
prop_allSubtrees f = True ==> all (==True) $ map (\subtree -> subSet (Set [subtree]) subF) ((\ (Set x) -> x) subF)
            where subF = sub f


nsub :: Form -> Int
nsub f = length $ (\ (Set x) -> x) (sub f)

-- test_nsub :: Form -> Property
-- test_nsub f = True ==>
