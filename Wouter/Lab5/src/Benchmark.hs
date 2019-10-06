module Benchmark

where
import Criterion.Main
import Exercise1

-- main :: IO ()
large = defaultMain [
    bench "exM" $ whnf (exM 40849302794 233333) 38029489032, 
    bench "exM2" $ whnf (exM2 40849302794 233333) 38029489032,
    bench "pow" $ whnf (power 40849302794 233333) 38029489032
  ]

small = defaultMain [
  bench "exM" $ whnf (exM 3 4) 2, 
  bench "exM2" $ whnf (exM2 3 4) 2,
  bench "pow" $ whnf (power 3 4) 2
  ]

largest = defaultMain [
  bench "exM" $ whnf (exM 408493027947798327 233333)   38029489032, 
  bench "exM2" $ whnf (exM2 408493027947798327 233333) 38029489032,
  bench "pow" $ whnf (power 408493027947798327 233333) 38029489032
  ]