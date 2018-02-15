module Benchmarks.Braille
  (benchmarks
  ) where

import Criterion.Main
import Braille

benchmarks =
  [ bgroup "Braille" [ bench "initCanvas_100x100" $ whnf (initCanvas 100) 100
                     , bench "toBounds" $ whnf (toBounds 0 100 (-200) 200) 50
                     , bench "prettyBounds"  $ whnf prettyBounds 12345
                     ]
  ]



