import Lab2
import Test.QuickCheck
import Data.List
import Data.Char
import Data.Ord

countBetween :: [Float] -> Float -> Float -> Int
countBetween list lo hi = length (filter (\x -> (x > lo && x <= hi)) list)

countQuartiles size = do
    y <- probs size
    print (show (countBetween y (-0.1) 0.25))
    print (show (countBetween y 0.25 0.5))
    print (show (countBetween y 0.5 0.75))
    print (show (countBetween y 0.75 1.0))
