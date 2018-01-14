module Lib
    ( runUi
    ) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Types (Size)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Lens.Micro ((^.))

import qualified Graphics.Vty as V

import System.Random (newStdGen, randomR, StdGen, getStdRandom)

import Data.Monoid ((<>))
import Data.Char
import Data.Bits (xor, (.|.))
import qualified Data.Vector as V

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)


type Row = V.Vector Int
type Canvas = V.Vector Row

data Tick = Tick Double Double 
type Name = ()

data CanvasState = CanvasState
  { canvas :: Canvas
  , width :: Int
  , height :: Int
  , points :: [(Double,Double)]
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  }

base = 0x2800
dots = [0x1,0x8,0x2,0x10,0x4,0x20,0x40,0x80]
brailleWidth = 2
brailleHeight = 4

toggleBit c x y =
  c `xor` dots !! (y*2+x)

setBit c x y =
  c .|. dots !! (y*2+x)

initCanvas :: Int -> Int -> Canvas
initCanvas w h =
  let
    row = V.replicate w base
    canvas = V.replicate h row
  in
    canvas

toBounds :: Double -> Double -> Double -> Double -> Double -> Double
toBounds min max min' max' v =
  let
    l  = abs $ max - min
    l' = abs $ max' - min'
    r  = (Prelude.min l l') / (Prelude.max l l')
  in
    abs $ ((abs min') + v) * r 

setDot :: CanvasState -> Double -> Double -> Canvas
setDot (CanvasState canvas w h _ xmin xmax ymin ymax) x y =
  let
    bx = floor $ toBounds 0.0 (fromIntegral w) xmin xmax x :: Int
    by = floor $ toBounds 0.0 (fromIntegral h) ymin ymax y :: Int
    (x',xDot)  = bx `quotRem` brailleWidth
    (y',yDot)  = by `quotRem` brailleHeight
    row = canvas V.! y'
    col = setBit (row V.! x') xDot (brailleHeight - yDot - 1)
    row' = row V.// [(x',col)]
    canvas' = canvas V.// [(y',row')]
  in
    if bx < w && by < h then
      canvas'
    else
      canvas
    
loop chan x y = do
  -- a <- getStdRandom (randomR (0, 1000)) :: IO Int
  -- b <- getStdRandom (randomR (0, 1000)) :: IO Int
  let y' = 100 * (sin $ x/30)
  writeBChan chan (Tick x y')
  threadDelay 100000
  loop chan (x+3) y

runUi :: IO()
runUi = do
  chan <- newBChan 10
  let c = initCanvasState 50 20 :: CanvasState
  let w = width c
  let h = height c
  forkIO $ loop chan 0.0 0.0
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app c

step :: Double -> Double -> CanvasState -> CanvasState
step a b c = c { points = points c ++ [(a,b)] }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, width = w, height = h, points = [], xMin = 0.0, xMax = 100.0, yMin = -200.0, yMax = 200.0 }

app :: App CanvasState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: CanvasState -> BrickEvent Name Tick -> EventM Name (Next CanvasState )
handleEvent c (AppEvent (Tick a b)) = continue $ step a b c
handleEvent c (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt c
handleEvent c (VtyEvent (V.EvKey V.KEsc []))        = halt c
handleEvent c (VtyEvent (V.EvResize w h))           = continue $ resize w h c
handleEvent c _                                     = continue c

drawUI :: CanvasState -> [Widget Name]
drawUI c = [vBox [canvasWidget c, padLeftRight 1 (str $ "Width: " ++ (show $ width c)) <+> padLeftRight 1 (str $ "Height: " ++ (show $ height c)) <+> padLeftRight 1 (str $ "X Min: " ++ (show $ xMin c)) <+> padLeftRight 1 (str $ "X Max: " ++ (show $ xMax c)) <+> padLeftRight 1 (str $ "Y Min: " ++ (show $ yMin c)) <+> padLeftRight 1 (str $ "Y Max: " ++ (show $ yMax c)) ]]

stupidRender width' height' cs = foldl (\c (x,y) -> c { canvas = setDot c x y }) (cs { canvas = initCanvas width' height', width = width'*brailleWidth, height = height'*brailleHeight }) (points cs)

canvasWidget :: CanvasState -> Widget n
canvasWidget cs =
  Widget Greedy Greedy $ do
      ctx <- getContext

      let width' = ctx^.availWidthL
      let height' = ctx^.availHeightL
      let c = stupidRender width' height' cs
      
      let width'' = (width cs) `quot` brailleWidth
      let height'' = (height cs) `quot` brailleHeight

      render $ C.center $ withBorderStyle BS.unicodeBold
             $ B.borderWithLabel (str "Plot")
             $ vBox (rows width' height' (map V.toList $ V.toList $ canvas c))
  where
    rows w h c    = [hBox $ cellsInRow w ( c !! r) | r <- [h-1,h-2..0]]
    cellsInRow w y  = [str $ map chr y]

theMap :: AttrMap
theMap = attrMap V.defAttr []