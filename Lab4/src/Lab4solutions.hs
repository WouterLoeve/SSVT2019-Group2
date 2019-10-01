module Lab4solutions where
import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import Control.Monad
import SetOrd
import System.Random
import Lecture4
import Debug.Trace

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
-}

{-
 - Testing from scratch
-}

{-
 - Generates a random set with x values between 0 and x.
-}
randomSet :: Int -> IO (SetOrd.Set Int)
randomSet x = do
    g <- newStdGen
    return $ list2set $ take x (randomRs (0, x) g :: [Int])

{-
 - Generates random sets with lengths 0 to 99.
-}
listRandomSetLin :: IO [Set Int]
listRandomSetLin = mapM randomSet [0 .. 10^2-1]

{-
 - Tests the properties with our random set generator.
-}
testSetOwnGen :: IO ()
testSetOwnGen = do
    print "------ Testing Set Operators Own Generator ------"
    ownGenTestProp prop_setNoDups "Checking whether no duplicates are present in the set" >>= putStrLn
    ownGenTestProp prop_setOrdered "Checking whether the set is ordered" >>= putStrLn


{-
 - Tests a given property for random test cases using the
 - set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp :: (SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp f name = liftM2 (testRunHelper name) numCases numPass
    where
        xs = listRandomSetLin
        results = map f <$> xs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - QuickCheck tests
-}

{-
 - Generates an arbitrary set of length s.
-}
arbSet :: (Ord a, Arbitrary a) => Int -> Gen (Set a)
arbSet s = do
    n <- choose (0, s)
    l <- vectorOf n arbitrary
    return $ list2set l

{-
 - Makes a sized version of the aforementioned arbSet arbitrary
    so no function has to be specified.
-}
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized arbSet

{-
 - Tests the properties with the quickcheck set generator.
-}
testSetProp :: IO ()
testSetProp = do
    print "------ Testing Set QuickCheck ------"
    print "Testing whether Sets contain duplicates"
    quickCheck prop_setNoDups

    print "Testing whether Sets are ordered"
    quickCheck prop_setOrdered

{-
 - Properties tests
-}

{-
 - A set should not contain duplicates.
-}
prop_setNoDups :: SetOrd.Set Int -> Bool
prop_setNoDups (Set a) = length a == length (nub a)

{-
 - A set is ordered.
-}
prop_setOrdered :: Set Int -> Bool
prop_setOrdered (Set a) = sort a == a

{-
 - Exercise 2
 - We devised a bunch of properties based on the mathematical set properties and
    used both the quickcheck generator and our own generator to test these.
 - We found that these properties cover a lot of behaviour even implementing combinations of the three functions,
 -}
setUnion, setIntersect, setDifference :: Ord a => Set a -> Set a -> Set a
{-
 - Takes the union of two sets. Implemented with append since the set logic takes out all duplicates
 -}
setUnion      (Set xs) (Set ys) = list2set (xs ++ ys)

{-
 - Takes the intersection of two sets.
 -}
setIntersect  (Set xs) (Set ys) = list2set (xs `intersect` ys)

{-
 - Takes the relative complement of two sets.
 -}
setDifference (Set xs) (Set ys) = list2set (xs \\ ys)

{-
 - Properties
 -}

 {-
 - (A n B) subSet (A u B)
 -}
prop_unionInter :: Set Int -> Set Int -> Bool
prop_unionInter xs ys = (xs `setIntersect` ys) `subSet` (xs `setUnion` ys)

-- Helper function to compare sets (because order might differ)
setEqual :: Eq a => Set a -> Set a -> Bool
setEqual (Set a) (Set b) = null (a \\ b) && null (b \\ a)

-- A u B == B u A
prop_setUnionCommutative :: Set Int -> Set Int -> Bool
prop_setUnionCommutative a b = setEqual (setUnion a b) (setUnion b a)

-- (A u B) u C == A u (B u C)
prop_setUnionAssociative :: Set Int -> Set Int -> Set Int -> Bool
prop_setUnionAssociative a b c = setUnion (setUnion a b) c == setUnion a (setUnion b c)

-- A u (B n C) == (A u B) n (A u C)
prop_setUnionDistributive :: Set Int -> Set Int -> Set Int -> Bool
prop_setUnionDistributive a b c = setUnion a (setIntersect b c) == setIntersect (setUnion a b) (setUnion a c)

-- A n B == B n A
prop_setIntersectCommutative :: Set Int -> Set Int -> Bool
prop_setIntersectCommutative a b = setEqual (setIntersect a b) (setIntersect b a)

-- (A n B) n C == A n (B n C)
prop_setIntersectAssociative :: Set Int -> Set Int -> Set Int -> Bool
prop_setIntersectAssociative a b c = setIntersect (setIntersect a b) c == setIntersect a (setIntersect b c)

-- A n (B u C) == (A n B) u (A n C)
prop_setIntersectDistributive :: Set Int -> Set Int -> Set Int -> Bool
prop_setIntersectDistributive a b c = setIntersect a (setUnion b c) == setUnion (setIntersect a b) (setIntersect a c)

{-
 - (A \ B) u B == A u B
 -}
prop_setUnionDifference :: Set Int -> Set Int -> Bool
prop_setUnionDifference a b = (a `setDifference` b) `setUnion` b == a `setUnion` b

{-
 - (A \ B) subSet A
 -}
prop_setUnionDifference2 :: Set Int -> Set Int -> Bool
prop_setUnionDifference2 a b = (a `setDifference` b) `subSet` a

{-
 - C \ (A n B) == (C \ A) u (C \ B)
 -}
prop_setDifferenceIntersectUnion :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceIntersectUnion a b c = (c `setDifference` (a `setIntersect` b)) == (c `setDifference` a) `setUnion` (c `setDifference` b)

{-
 - C \ (A u B) == (C \ A) n (C \ B)
 -}
prop_setDifferenceUnionIntersect :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceUnionIntersect a b c = c `setDifference` (a `setUnion` b) == (c `setDifference` a) `setIntersect` (c `setDifference` b)

prop_setDifferenceDist :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceDist a b c = c `setDifference` (b `setDifference` a) == (c `setIntersect` a) `setUnion` (c `setDifference` b)

{-
 - (B \ A) n C == (B n C) \ A == B n (C \ A)
 -}
prop_setDifferenceDiff :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceDiff a b c = (b `setDifference` a) `setIntersect` c == (b `setIntersect` c) `setDifference` a

{-
 - (B \ A) u C == (B u C) \ (A \ C)
 -}
prop_setDifferenceUnion :: Set Int -> Set Int -> Set Int -> Bool
prop_setDifferenceUnion a b c = (b `setDifference` a) `setUnion` c == (b `setUnion` c) `setDifference` (a `setDifference` c)

{-
 - A \ A = 0
 - 0 = Empty set
 -}
prop_setDifferenceIdentity :: Set Int -> Bool
prop_setDifferenceIdentity a = (a `setDifference` a) == list2set []
{-
 - 0 \ A = 0
 -}
prop_setDifferenceEmpty :: Set Int -> Bool
prop_setDifferenceEmpty a = (list2set []) `setDifference` a == list2set []

{-
 - A \ 0 = A
 -}
prop_setDifferenceEmpty2 :: Set Int -> Bool
prop_setDifferenceEmpty2 a = a `setDifference` (list2set []) == a

{-
 - Quickcheck test caller using generator from exercise 1.
 -}
testSetOperators = do
    print "------ Testing Set Operators QuickCheck ------"
    print "Testing whether intersection is a subset of the union"
    quickCheck prop_unionInter
    print "Testing union commutative property"
    quickCheck prop_setUnionCommutative
    print "Testing union associative property"
    quickCheck prop_setUnionAssociative
    print "Testing union distributive property"
    quickCheck prop_setUnionDistributive

    print "Testing intersection commutative property"
    quickCheck prop_setIntersectCommutative
    print "Testing intersection associative property"
    quickCheck prop_setIntersectAssociative
    print "Testing intersection distributive property"
    quickCheck prop_setIntersectDistributive
    print "Testinging whether difference of two sets unioned with the removed parts equals the union."
    quickCheck prop_setUnionDifference
    print "Testinging whether the difference between a and b is a subset of the original"
    quickCheck prop_setUnionDifference2

    print "Testing Difference distributative intersection property "
    quickCheck prop_setDifferenceIntersectUnion
    print "Testing Difference distributative union property"
    quickCheck prop_setDifferenceUnionIntersect
    print "Testing Difference distributative property"
    quickCheck prop_setDifferenceDist
    print "Testing Difference commutative intersect property"
    quickCheck prop_setDifferenceDiff
    print "Testing Difference commutative union property"
    quickCheck prop_setDifferenceUnion
    print "Testing Difference identity"
    quickCheck prop_setDifferenceIdentity
    print "Testing Difference Empty and A"
    quickCheck prop_setDifferenceEmpty
    print "Testing Difference A and empty"
    quickCheck prop_setDifferenceEmpty2


{-
 - Tests a given property for two sets with random test
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp2 :: (SetOrd.Set Int -> SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp2 f name = liftM2 (testRunHelper name) numCases numPass
    where
        xs = listRandomSetLin
        ys = listRandomSetLin
        results = zipWith f <$> xs <*> ys
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - Tests a given property for three sets with random test
    cases of the set of ints defined in SetOrd.
 - Prints appropriate test results.
-}
ownGenTestProp3 :: (SetOrd.Set Int -> SetOrd.Set Int -> SetOrd.Set Int -> Bool) -> String -> IO String
ownGenTestProp3 f name = liftM2 (testRunHelper name) numCases numPass
    where
        xs = listRandomSetLin
        ys = listRandomSetLin
        zs = listRandomSetLin
        results = zipWith3 f <$> xs <*> ys <*> zs
        numPass = length . filter (==True) <$> results
        numCases = length <$> results

{-
 - Caller for the own generator properties
-}
testSetOperatorsOwnGen :: IO ()
testSetOperatorsOwnGen = do
    print "------ Testing Set Operators Own Generator ------"
    ownGenTestProp2 prop_unionInter "Testing whether intersection is a subset of the union" >>= putStrLn
    ownGenTestProp2 prop_setUnionCommutative "Testing union commutative property" >>= putStrLn
    ownGenTestProp3 prop_setUnionAssociative "Testing union associative property" >>= putStrLn
    ownGenTestProp3 prop_setUnionDistributive "Testing union distributive property" >>= putStrLn
    ownGenTestProp2 prop_setUnionDifference "Testing whether difference of two sets unioned with the removed parts equals the union." >>= putStrLn
    ownGenTestProp2 prop_setUnionDifference2 "Testing whether the difference between a and b is a subset of the original" >>= putStrLn

    ownGenTestProp2 prop_setIntersectCommutative "Testing intersection commutative property" >>= putStrLn
    ownGenTestProp3 prop_setIntersectAssociative "Testing intersection associative property" >>= putStrLn
    ownGenTestProp3 prop_setIntersectDistributive "Testing intersection distributive property" >>= putStrLn

    ownGenTestProp3 prop_setDifferenceIntersectUnion "Testing Difference distributative intersection property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceUnionIntersect "Testing Difference distributative union property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceDist "Testing Difference distributative property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceDiff "Testing Difference commutative intersect property" >>= putStrLn
    ownGenTestProp3 prop_setDifferenceUnion "Testing Difference commutative union property" >>= putStrLn
    ownGenTestProp prop_setDifferenceIdentity "Testing Difference identity" >>= putStrLn
    ownGenTestProp prop_setDifferenceEmpty "Testing Difference Empty and A" >>= putStrLn
    ownGenTestProp prop_setDifferenceEmpty2 "Testing Difference A and empty" >>= putStrLn

{-
 - Exercise 3
-}
type Rel a = [(a,a)]

{-
 - A symmetric closure S of a relation R is defined as the union of R with it's converse relation.
 - The converse relation is defined as {(x,y) : (y,x) in R}. This means that we swap the two variables
 - in every tuple in R, so (1,2) becomes (2,1). The below implementation does exactly this:
 - 1. map and 'swap' are used to create the converse relation of R
 - 2. a union is made of R with the converse relation
 - 3. the result is sorted
 -}
symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` (swap <$> r)

{-
 - Exercise 4
 - For this problem we defined a few generators to help us in our testing. 
 - We first test our generators and then our defined properties.

 - Question 3: The relation R = {(x,y) | x = y mod n} is serial for domain A if:
    for all x in A, 0 <= x < n and (x,x) is in R <=>  R is reflexive for A
    Reasoning:
      since 0 <= (y mod n) < n and x = (y mod n), x cannot be the left hand side of the relation if x < 0 or x >= n
      since y = x + k * n and y < n (else there must be z s.t. y = z mod n which is impossible), k = 0 and y = x
    Since R should be reflexive we can test if reflexiveness leads to seriality.
-}

{-
 - Check whether a relation is serial.
 -}
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial dom rel = all (\x -> or [(x, y) `elem` rel | y <- dom]) dom

{-
 - Check whether a relation is reflexive
 -}
isReflexive :: [Int] -> Rel Int -> Bool
isReflexive dom rel = all (\x -> (x, x) `elem` rel) dom

{-
 - Generates reflexive relations
 -}
arbReflexive :: Int -> Gen ([Int], Rel Int)
arbReflexive s = do
    n <- choose (0, s)
    l <- vector n
    let dom = nub l
    return (dom, zip dom dom)

{-
 - Generates serial relations
 -}
arbSerial :: Int -> Gen ([Int], Rel Int)
arbSerial s = do
    n <- choose (0, s)
    dom <- vector n
    b <- vectorOf n $ elements dom
    return (dom, zip dom b)

{-
 - Test whether generator actually gives serial relations because we use it later for other functions.
 -}
prop_testArbSerialGen :: [Int] -> Rel Int -> Bool
prop_testArbSerialGen = isSerial

{-
 - Test whether generator actually gives serial relations because we use it later for other functions.
 -}
prop_testArbRefGen :: [Int] -> Rel Int -> Bool
prop_testArbRefGen = isReflexive

{-
 - A reflexive function is always serial
 -}
prop_reflexiveIsSerial :: [Int] -> Rel Int -> Property
prop_reflexiveIsSerial dom rel = isReflexive dom rel ==> isSerial dom rel

{-
 - Testing if all first elements of the relations are in the domain
 -}
prop_serialDomainCoverage :: [Int] -> Rel Int -> Bool
prop_serialDomainCoverage dom rel =  all (`elem` (fst <$> rel)) dom

{-
 - You could also build the following property.
 - However we think that the above 2 provide more coverage 
    (in a situation were we could give only 2 properties)
 - Since the property is almost the same as checking that the relation is universal.
 - Also the property only works when all relations are actually on the domain.
 -}
prop_serialDomainLength :: [Int] -> Rel Int -> Property
prop_serialDomainLength dom rel = length dom * (length dom - 1) < length rel ==> isSerial dom rel

testIsSerial :: IO ()
testIsSerial = do
    print "Testing if generator return serial relations"
    quickCheck $ forAll (arbSerial 10) (uncurry prop_testArbSerialGen)

    print "Testing if generator return reflexive relations"
    quickCheck $ forAll (arbReflexive 10) (uncurry prop_testArbRefGen)

    print "Testing if reflexive relations are serial"
    quickCheck $ forAll (arbReflexive 10) (uncurry prop_reflexiveIsSerial)
    
    print "Testing if all first elements of the relations are in the domain"
    quickCheck $ forAll (arbSerial 10) (uncurry prop_serialDomainCoverage)
{-
 - Exercise 5
 - Compute the transitive closure by recursively taking the union with the 
    relation composition until a fixed point is reached.
-}

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos r = sort $ fix (\ f s -> if s == unComp s then s else f $ unComp s) r
    where unComp s = s `union` (r @@ s)

{-
 - Exercise 6
-}

{-
 - Because the symmetric closure of a relation R is always the union of R and it's conver relation,
 - we know the lower and upper size bound of the output of our symClos function.
 -
 - The size of the output must always be greater or equal than the size of the input.
 - Why equal? consider the example of an empty relation `Rel []` and an already symmetric relation
 - such as `Rel [(1,2), (2,1)]`. Either example will not be changed by the function, and their output
 - will therefore be of the same length as the input.
 -
 - The size of the output must always be smaller than or equal than the size of the input times two.
 - Here, equal is included to handle cases such as `Rel [(1,2), (3,4)]` which will have output
 - `Rel [(1,2), (3,4), (2,1), (4,3)]` which is exactly twice the size
 - (therefore satisfying the test 4 <= 2*2). The output length of symClos will be shorter if there are
 - duplicates.
 -}
prop_symClosLengthMin :: Rel Int -> Bool
prop_symClosLengthMin r = length (symClos r) >= length r

prop_symClosLengthMax :: Rel Int -> Bool
prop_symClosLengthMax r = length (symClos r) <= 2 * length r

{-
 - Function to check if a relation is Symmetric
 -}
isSymRel :: Rel Int -> Bool
isSymRel r = and [(b, a) `elem` r | (a, b) <- r]

{-
 - Here we use the property that the output of symClos must be symmetric
 -}
prop_symClosIsSymRel :: Rel Int -> Bool
prop_symClosIsSymRel r = isSymRel (symClos r)

{-
 - Property that symClos should do nothing to already symmetric sets
 - Here we sort the given list so we can compare it with the output of the symmetric closure.
 -}
prop_symUnaffected :: Rel Int -> Property
prop_symUnaffected r = isSymRel r ==> symClos r == sort r

{-
 - Test function
 -}
testSymClos = do
    print "Testing symmetric closure length of R >= length of R"
    quickCheck prop_symClosLengthMin
    print "Testing symmetric closure length of R <= 2* length of R"
    quickCheck prop_symClosLengthMax
    print "Testing that output of symClos is symmetric"
    quickCheck prop_symClosIsSymRel
    print "Testing that symmetric sets are unaffected by symClos"
    quickCheckWith stdArgs { maxSize = 10 } prop_symUnaffected

{-
 - While not as straightforward as the maximum size of an union, we can also use the upper and lower bound of the
 - transitive closure in our quickCheck tests.
 - According to the paper "Distributed Algorithms for the Transitive Closure" (Eric Gribkoff, 2013) the upper
 - bound of a transitive closure on a set with length l is l^2
 - Again, we use >= and <= instead of > and < for the same reasons explained in the case of symClos.
 -}

prop_trClosLengthMin :: Rel Int -> Bool
prop_trClosLengthMin r = length (trClos r) >= length r

prop_trClosLengthMax :: Rel Int -> Bool
prop_trClosLengthMax r = length (trClos r) <= length r ^ 2

{-
 - Function to check if a relation is Transitive
 -}

isTrRel :: Rel Int -> Bool
isTrRel r = and [(a,c) `elem` r | (a,b) <- r, (b',c) <- r, b' == b]

{-
 - Here we use the property that the output of trClos must be transitive
 -}
prop_trClosIsTrRel :: Rel Int -> Bool
prop_trClosIsTrRel r = isTrRel (trClos r)

{-
 - Here we use the property that the intersection of two transitive sets must itself be transitive.
 - If we would just pass two transitive tests, we would not be testing the trClos function.
 - Therefore, we only input one set that has a precondition of being transitive, and one set without precondition.
 - If our trClos function is correct, we would expect the intersection of the transitive set and the set produced by
 - trClos of the second set, to also be transitive.
 -}
prop_trIntersection :: Rel Int -> Rel Int -> Property
prop_trIntersection ra rb = isTrRel ra ==> isTrRel $ ra `intersect` trClos rb

{-
 - Property that trClos should do nothing to already transitive sets.
 - We sort the given relation by the generator to give us the ability to compare.
 -}
prop_trUnaffected :: Rel Int -> Property
prop_trUnaffected r = isTrRel r ==> trClos r == sort r

{-
 - Test function
 -}
testTrClos = do
    print "Testing transitive closure length of R >= length of R"
    quickCheck prop_trClosLengthMin
    print "Testing transitive closure length of R <= length of R ^ 2"
    quickCheck prop_trClosLengthMax
    print "Testing that output of trClos is transitive"
    quickCheck prop_trClosIsTrRel
    print "Testing that transitive sets are unaffected by trClos"
    quickCheckWith stdArgs { maxSize = 10 } prop_trUnaffected

    print "Testing intersection of transitive sets"
    quickCheckWith stdArgs { maxSize = 10 } prop_trIntersection

{-
 - We also do some small unit testing to check if our function works on known cases
 -}
trClosCases = [
    ([],                         []),
    ([(0,1)],                    [(0,1)]),
    ([(0,1), (1,2)],             [(0,1),(0,2),(1,2)]),
    ([(0,1), (1,0)],             [(0,0),(0,1),(1,0),(1,1)]),
    ([(0,1), (1,0), (1,2)],      [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]),
    ([(0,1), (1,2), (2,0)],      [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)])
    ]

testTrClos2 = do
    let numCases = length trClosCases
    let numPass = length (filter (\v -> v) [trClos a == b | (a, b) <- trClosCases])
    print $ testRunHelper "trClos: known cases" numCases numPass


{-
 - Exercise 7
 - Time: 30 min
 - The relation R = [(a,b)] where b =/= a results into different results for
 - the symmetric closure of the transitive closure and the transitive closure
 - of the symmetric closure.
 -
 - Let [(a,b)] be (1,2). The transitive closure of R is still [(1,2)].
 - So the symmetric closure of this transitive closure will be [(1,2), (2,1)]
 -
 - Doing the same thing for the opposite, the symmetric closure is [(1,2), (2,1)]
 - So the transitive closure of this symmetric closure will be [(1,1), (1,2), (2,1), (2,2)]
 -
 - As we can see, these two statements are not the same, and this is illustrated
 - programmatically below.
-}
illustrateDiff = do
    print $ "Symmetric closure of the transitive closure " ++ show (symClos $ trClos [(1,2)])
    print $ "Transitive closure of the symmetric closure " ++ show (trClos $ symClos [(1,2)])

{-
 - Exercise 8
 - We could not finish everything.
 -}

instance Show Expr where
    show (I num) = show num
    show (V var) = var
    show (Add expr1 expr2) = show expr1 ++ " + " ++ show expr2
    show (Subtr expr1 expr2) = show expr1 ++ " - " ++ show expr2
    show (Mult expr1 expr2) = show expr1 ++ " * " ++ show expr2

instance Show Condition where
    show (Prp var) = var
    show (Eq expr1 expr2) = show expr1 ++ " == " ++ show expr2
    show (Lt expr1 expr2) = show expr1 ++ " < " ++ show expr2
    show (Gt expr1 expr2) = show expr1 ++ " > " ++ show expr2
    show (Ng cond) = "!(" ++ show cond ++ ")"

instance Show Statement where
    show (Ass var expr) = var ++ " = " ++ show expr
    show (Cond cond stmt1 stmt2) = "if (" ++ show cond ++ ") then {" ++ show stmt1 ++ "} else {" ++ show stmt2 ++ "}"
    show (Seq stmts) = intercalate "; " (show <$> stmts) ++ ";"
    show (While cond stmt) = "while (" ++ show cond ++ ") do {" ++ show stmt ++ "}"

instance Read Expr where
    readsPrec d r = readParen (d > app_prec)
                    (\r -> [(I (read i), r') |
                            (i, r') <- lex r,
                            all isDigit i]) r
                ++ readParen (d > app_prec)
                    (\r -> [(V v, r') |
                            (v, r') <- lex r,
                            all isAlpha v]) r
                ++ readParen (d > up_prec)
                    (\r -> [(Add a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            ("+", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                ++ readParen (d > up_prec)
                    (\r -> [(Subtr a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            ("-", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                ++ readParen (d > up_prec)
                    (\r -> [(Mult a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            ("*", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                where app_prec = 10
                      up_prec = 5


instance Read Condition where
    readsPrec d r = readParen (d > app_prec)
                    (\r -> [(Prp v, r') |
                            (v, r') <- lex r,
                            all isAlpha v]) r
                ++ readParen (d > up_prec)
                    (\r -> [(Eq a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            ("==", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                ++ readParen (d > up_prec)
                    (\r -> [(Lt a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            ("<", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                ++ readParen (d > up_prec)
                    (\r -> [(Gt a b, r''') |
                            (a, r') <- readsPrec (up_prec + 1) r,
                            (">", r'') <- lex r',
                            (b, r''') <- readsPrec (up_prec + 1) r'']) r
                ++ readParen (d > neg_prec)
                    (\r -> [(Ng c, r'''') |
                            ("!", r') <- lex r,
                            ("(", r'') <- lex r',
                            (c, r''') <- readsPrec (neg_prec + 1) r'',
                            (")", r'''') <- lex r''']) r
                where app_prec = 10
                      up_prec = 5
                      neg_prec = 0

instance Read Statement where
    readsPrec d r = (\r -> [(Ass v e, r''') |
                            (v, r') <- lex r,
                            ("=", r'') <- lex r',
                            (e, r''') <- reads r'']) r
                ++ (\r -> [(Cond c a b, r12) |
                            ("if", r1) <- lex r,
                            ("(", r2) <- lex r1,
                            (c, r3) <- reads r2,
                            (")", r4) <- lex r3,
                            ("then", r5) <- lex r4,
                            ("{", r6) <- lex r5,
                            (a, r7) <- reads r6,
                            ("}", r8) <- lex r7,
                            ("else", r9) <- lex r8,
                            ("{", r10) <- lex r9,
                            (b, r11) <- reads r10,
                            ("}", r12) <- lex r11]) r
                where app_prec = 10
                      up_prec = 5
                      neg_prec = 0





{-
 - Exercise 9
 -}

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

getSquare :: Int -> Int
getSquare n = sum (map (^2) (digs n))

getChain :: Int -> Int
getChain n = if n == 1 || n == 89 then n else getChain (getSquare n)

{-
 - While not the most elegant solution, we can calculate the number of chains that end in 89 by bruteforcing.
 -}

euler92 :: Int
euler92 = length (filter (==89) [getChain x | x <- [1..10000000]])
          
          