module Benchmark

where
import Criterion.Main
import Exercise1

-- main :: IO ()
temp = defaultMain [
    bench "exM" $ whnf (exM 40849302794 233333) 38029489032,
    bench "exM2" $ whnf (exM2 40849302794 233333) 38029489032,
    bench "pow" $ whnf (power 40849302794 233333) 38029489032
  ]

-- temp = defaultMain [
--     bench "exM" $ whnf (exM 100 33) 2,
--     bench "pow" $ whnf (power 100 33) 2
--   ]

-- temp = defaultMain [
--    bench "exp" $ whnf exp (2 :: Double)
--  , bench "log" $ whnf log (2 :: Double)
--  , bench "sqrt" $ whnf sqrt (2 :: Double)
--  ]