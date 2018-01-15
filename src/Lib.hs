{-# LANGUAGE RecordWildCards #-}

module Lib
    ( runUi
    ) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Types (Size)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Text.Wrap (WrapSettings(..))
import Lens.Micro ((^.))

import qualified Graphics.Vty as V

import System.Random (newStdGen, randomR, StdGen, getStdRandom)

import Data.Monoid ((<>))
import Data.Char
import Data.Bits (xor, (.|.))
import qualified Data.Vector.Unboxed as V

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import qualified System.IO as IO
import qualified System.Environment as Env

type Canvas = V.Vector Int

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
    canvas = V.replicate (w*h) base
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

prettyBounds :: Double -> Double
prettyBounds v =
  let
    s = signum v
    v' = abs v
    t = 10 ^ (floor $ logBase 10.0 v') :: Int
    n = t * (ceiling $ v' / fromIntegral t)
  in
    if v' < 1 then
      v
    else if v' > 2 && v' < 10 then
      s * 10
    else
      1.2 * s * fromIntegral (n)

setDots :: CanvasState -> Canvas
setDots (CanvasState canvas w h ps xmin xmax ymin ymax) =
  let
    dots (x,y) =
      let
        bx = round $ toBounds 0.0 (fromIntegral w) xmin xmax x :: Int
        by = h - (round $ toBounds 0.0 (fromIntegral h) ymin ymax y) :: Int
        (x',xDot)  = bx `quotRem` brailleWidth
        (y',yDot)  = by `quotRem` brailleHeight
        (x'', xDot') = if x' > (w `div` brailleWidth - 1) then
                         (w `div` brailleWidth- 1, brailleWidth -1)
                       else
                         (x', xDot)
        (y'', yDot') = if y' > (h `div` brailleHeight - 1) then
                         (h `div` brailleHeight - 1, brailleHeight -1)
                       else
                         (y', yDot)
        i = (w `div` brailleWidth)*y'' + x''
      in
        (i, (xDot', yDot'))
    ds = map dots ps
    canvas' = V.accum (\v (x,y) -> setBit v x y) canvas ds
  in
    if w > 0 && h > 0 then
      canvas'
    else
      canvas
 
-- loop chan f x y = do
--   -- a <- getStdRandom (randomR (0, 1000)) :: IO Int
--   -- n <- getStdRandom (randomR (-100.0, 100.0)) :: IO Double
--   let y' = (100 * (sin $ x/100))
--   --let y' = y+1
--   writeBChan chan (Tick x y')
--   threadDelay 100000
--   loop chan f (x+30) y'
-- 

loop chan h x y = do
  o <- IO.hIsOpen h
  if o then do
    l <- IO.hGetLine h

    writeBChan chan (Tick x (read l :: Double))
    loop chan h (x+1) y
  else
    return ()

runUi :: IO()
runUi = do
  chan <- newBChan 100
  let c = initCanvasState 50 20 :: CanvasState
  let w = width c
  let h = height c
  f <- Env.getArgs
  h <- IO.openFile (f !! 0) IO.ReadMode
  forkIO $ loop chan h 0.0 0.0
  void $ customMain (V.mkVty V.defaultConfig ) (Just chan) app c

  IO.hClose h

step :: Double -> Double -> CanvasState -> CanvasState
step x y c@CanvasState{..} = c { points = points ++ [(x,y)], xMin = min (prettyBounds x) xMin, xMax = max (prettyBounds x) xMax, yMin = min (prettyBounds y) yMin, yMax = max (prettyBounds y) yMax }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
--initCanvasState w h = CanvasState { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight, points = [(100.0,0.0), (50.0,200.0), (150.0,-200.0)], xMin = 0.0, xMax = 10.0, yMin = 0.0, yMax = 10.0 }
initCanvasState w h = CanvasState { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight, points = [], xMin = 0.0, xMax = 10.0, yMin = 0.0, yMax = 10.0 }

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
handleEvent c (VtyEvent (V.EvResize w h))           = continue $ resize (w-2) (h-2) c
handleEvent c _                                     = continue c

drawUI :: CanvasState -> [Widget Name]
drawUI c = [vBox [canvasWidget c, padLeftRight 1 (str $ "Width: " ++ (show $ width c)) <+> padLeftRight 1 (str $ "Height: " ++ (show $ height c)) <+> padLeftRight 1 (str $ "X Min: " ++ (show $ xMin c)) <+> padLeftRight 1 (str $ "X Max: " ++ (show $ xMax c)) <+> padLeftRight 1 (str $ "Y Min: " ++ (show $ yMin c)) <+> padLeftRight 1 (str $ "Y Max: " ++ (show $ yMax c)) ]]

renderCanvas cs = setDots cs

wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
canvasWidget :: CanvasState -> Widget n
canvasWidget cs =
  Widget Greedy Greedy $ do
      ctx <- getContext

      let width' = ctx^.availWidthL - 2
      let height' = ctx^.availHeightL - 2
      let c = renderCanvas $ cs { canvas = initCanvas width' height', width = width'*brailleWidth, height = height'*brailleHeight }
      
      render $ C.center $ withBorderStyle BS.unicodeBold
             $ B.borderWithLabel (str "Plot")
             $ vBox (rows width' height' (V.toList $ c))
  where
    rows w h c    = [strWrapWith wrapSettings $ map chr c]

theMap :: AttrMap
theMap = attrMap V.defAttr []