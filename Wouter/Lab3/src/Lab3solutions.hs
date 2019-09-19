module Lab3solutions where
import Data.List
import Data.Char
import Data.Tuple
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
getTruthTable f = map (\ v -> evl v f) (allVals f)

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
 - Time:  min
 - Options:
    - use Format's to convert to String with Show, Parse them and use equiv.
    - make seperate Format's and String notations and then use equiv
    - Same as the above but then compare strings
    - Instead of equiv check if contradiction, entails and tautology yield the same results
-}


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
 - Time: 30 min
 - A: 
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
        quickCheck (forAll genForms prop_equivCNF)
        quickCheck (forAll genForms prop_tautCNF)
        quickCheck (forAll genForms prop_contraCNF)
        quickCheck (forAll genForms prop_rEntailsCNF)
        quickCheck (forAll genForms prop_lEntailsCNF)
        quickCheck (forAll genForms prop_followsGrammar)

-- Todo testing report + prints with each test