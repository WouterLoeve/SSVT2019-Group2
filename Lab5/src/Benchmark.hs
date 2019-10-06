module Benchmark

where
import Criterion.Main
import Lecture5
import Test.QuickCheck

large = defaultMain [
    bench "exM" $ whnf (exM 40849302794 233333) 38029489032, 
    bench "expM" $ whnf (expM 40849302794 233333) 38029489032
  ]

small = defaultMain [
  bench "exM" $ whnf (exM 3 4) 2, 
  bench "expM" $ whnf (expM 3 4) 2
  ]

largest = defaultMain [
  bench "exM" $ whnf (exM 408493027947798327 233333)   38029489032, 
  bench "expM" $ whnf (expM 408493027947798327 233333) 38029489032
  ]
