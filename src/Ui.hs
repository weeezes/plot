{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Ui 
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

import qualified Options.Applicative as Options

import Data.Monoid ((<>))
import Data.Char
import qualified Data.Vector.Unboxed as V

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A

import Types
import Braille

type Name = ()

parseSecondValue :: A.Parser Double
parseSecondValue = do
  A.skipSpace
  A.double

parsePoint :: A.Parser Point
parsePoint = do
  x <- A.double
  y <- A.choice [A.skipSpace >>= \_ -> A.endOfLine >>= \_ -> return 0.0, parseSecondValue]
  return $ Point x y

loop chan h = do
  o <- IO.hIsOpen h
  eof <- IO.hIsEOF h
  if o && not eof then do
    l <- BS.hGetLine h
    case A.parseOnly parsePoint l of
      Right v -> writeBChan chan v
      Left _ -> return ()
    loop chan h
  else
    return ()

loopSingles chan h x = do
  o <- IO.hIsOpen h
  eof <- IO.hIsEOF h
  if o && not eof then do
    l <- BS.hGetLine h
    case A.parseOnly parseSecondValue l of
      Right v -> writeBChan chan $ Point x v
      Left _ -> return ()
    loopSingles chan h (x+1)
  else
    return ()

settingsParser :: Options.Parser Settings
settingsParser = Settings
  <$> Options.argument Options.str (Options.metavar "FD")
  <*> Options.switch (Options.long "singles" <> Options.short 's')

settingsParserInfo :: Options.ParserInfo Settings
settingsParserInfo = Options.info (settingsParser Options.<**> Options.helper) 
                                  (Options.progDesc "Plot stuff on the terminal.")

runUi :: IO()
runUi = do
  chan <- newBChan 100
  let c = initCanvasState 50 20 :: CanvasState
  let w = width c
  let h = height c
  settings <- Options.execParser settingsParserInfo
  let f = inputStream settings
  exists <- D.doesFileExist f
  if exists then do
    h <- IO.openFile f IO.ReadMode
    if singles settings then do
      forkIO $ loopSingles chan h 0
      void $ customMain (V.mkVty V.defaultConfig ) (Just chan) app c
    else do
      forkIO $ loop chan h
      void $ customMain (V.mkVty V.defaultConfig ) (Just chan) app c

    IO.hClose h
  else
    putStrLn "Given file descriptor doesn't exist"

step :: Double -> Double -> CanvasState -> CanvasState
step x y c@CanvasState{..} = c { points = points ++ [(x,y)], xMin = min (prettyBounds x) xMin, xMax = max (prettyBounds x) xMax, yMin = min (prettyBounds y) yMin, yMax = max (prettyBounds y) yMax }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight, points = [], xMin = 0.0, xMax = 0.0, yMin = 0.0, yMax = 0.0 }

app :: App CanvasState Point Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: CanvasState -> BrickEvent Name Point -> EventM Name (Next CanvasState )
handleEvent c (AppEvent (Point x y)) = continue $ step x y c
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