import Criterion.Main

import qualified Benchmarks.Braille as Braille
import qualified Benchmarks.CanvasState as CanvasState

main = defaultMain $
  Braille.benchmarks ++
  CanvasState.benchmarks