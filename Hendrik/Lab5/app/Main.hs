import Text.Printf
import Data.List
import Test.QuickCheck
import Data.Char
import Lab5solutions

main :: IO ()
main = do
    leastComposites
    leastComposites'
    -- print $ expectedPassRate 1 1