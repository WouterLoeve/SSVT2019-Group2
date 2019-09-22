module Lab3solutions where
import Data.List
import Data.Char
import Data.Tuple
import SetOrd
import Text.Show.Functions
import Test.QuickCheck
import Control.Monad
import Lecture3

{- E1
 -}

contradiction, tautology :: Form -> Bool
contradiction = not . satisfiable
tautology f = all (\ v -> evl v f) (allVals f)

entails, equiv :: Form -> Form -> Bool
f `entails` g = tautology $ Impl g f
f `equiv` g = tautology $ Equiv f g

{- Testing:
 - Hardcoded test cases for one
 - Obvious truths (i.e. transforming the Form into a trivially equivalent one)
 -  - How do we do this for entailment? Adding an extra Cnj to g? Dsj to f?
 -  - Like swap order of Cnj Dsj Equiv, double Neg etc.
 - CNF/DNF may be able to assist in this (but depends on correctness of those).
 -}

{- E2
 -}

{- Unit testing ofc
 - Form -> show -> parse?
 - String -> Form -> show
 - Use QuickCheck or random generator from 4?
 -}

{- E3
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

{- E4
 - Test that the CNF is equivalent to the original Form
 - Test that the CNF has the expected structure
 - More ???
 - Postconditions of toCNF, other properties can only be stronger?
 - Note: should test generator?
 -}

prop_equivToCNF :: Form -> Bool
prop_equivToCNF f = f `equiv` toCNF f

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

{- E5
 -}

-- stock --
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
 
-- 1 --
substring :: String -> String -> Bool
substring x y@(_:t) = prefix x y || substring x t
substring _ _ = False

prefix :: String -> String -> Bool
prefix x y = x == take (length x) y

-- note: this should be tested too for the overall test to be valid
isSubForm :: Form -> Form -> Bool
isSubForm f g = substring (show f) (show g)

prop_areSubs :: Form -> Bool
prop_areSubs f = all (`isSubForm` f) $ (\ (Set l) -> l) (sub f)

-- 2 --
-- nsub :: Form -> Int
-- nsub f = 

{- E6 BONUS
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

