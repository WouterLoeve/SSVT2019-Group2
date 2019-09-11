import Lab2
import Test.QuickCheck
import Data.List
import Data.Char

-- Excercise 1: random in open interval
--countOccurence :: [Float] -> Int
--countOccurence n = do
--    l <- [x | x <- n, x>0.25, x<=0.5]
--    return (length l)

--testProportionInRange = do
--    a <- (probs 1000)
--    return $ (countOccurence a)

getQuartile :: [Float] -> Float -> Float -> Int
getQuartile list low high = length (filter (\x -> x>low && x<= high) list)
getQuartiles :: [Float] -> Int
getQuartiles list n =
    (probs n >>= )
--probs 1000 >>= \y -> return (getQuartile y 0.25 0.5)
-- (getQuartile[0, 1/4 .. 1]) <$> probs 1000
