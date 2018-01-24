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
import Data.Maybe (catMaybes)
import Data.Char
import qualified Data.Vector.Unboxed as V

import Control.Monad (forever, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM(..), atomically)

import qualified System.IO as IO
import qualified System.Directory as D
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Process as Process
import qualified System.Posix.IO as PIO
import System.Posix.Types (Fd)
import qualified System.Exit as Exit

import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA

import Types
import Braille

type Name = ()
data ParsedPoint = ParsedPoint Double Double | ParsedSingle Double

parsePoint :: A.Parser ParsedPoint
parsePoint = do
  A.skipSpace
  x <- A.signed A.double
  A.takeWhile (\c -> not (A.isDigit c || c == '.' || c == '-' || c == '\n' || c == '\r'))
  possiblyDigit <- A.peekChar
  case possiblyDigit of
    Just c -> 
      if A.isDigit c || c == '-' then do
        y <- A.signed A.double
        return $ ParsedPoint x y
      else
        return $ ParsedSingle x
    Nothing -> return $ ParsedSingle x

foldPoints queue acc (ParsedPoint x y) = do
  atomically $ writeTQueue queue $ Point x y
  return $ acc + 1
foldPoints queue acc (ParsedSingle y) = do
  atomically $ writeTQueue queue $ Point (acc+1) y
  return $ acc + 1

loop queue h x = do
  PP.evalStateT (PP.foldAllM (foldPoints queue) (return $ 0 :: IO Double) pure) $ PA.parsed parsePoint (PB.fromHandle h)
  return ()

redraw chan queue = do
  forever $ do
    ps <- sequence (replicate 1000 $ atomically $ tryReadTQueue queue) >>= \d -> return $ catMaybes d 
    if length ps > 0 then
      writeBChan chan $ Redraw ps
    else
      return ()
    threadDelay 30000
  return ()

settingsParser :: Options.Parser Settings
settingsParser = Settings
  <$> Options.argument Options.str (Options.metavar "FD")

settingsParserInfo :: Options.ParserInfo Settings
settingsParserInfo = Options.info (settingsParser Options.<**> Options.helper) 
                                  (Options.progDesc "Plot stuff on the terminal.")

handleShutdown :: V.Vty -> ThreadId -> Fd -> IO ()
handleShutdown vty tid fd = do
  putStrLn "Received SIGTERM, shutting down."
  -- TODO exit with proper error code if any of these fail
  killThread tid
  PIO.closeFd fd
  V.shutdown vty
  Process.exitImmediately Exit.ExitSuccess

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
    fd <- PIO.openFd f PIO.ReadOnly Nothing PIO.defaultFileFlags
    h <- PIO.fdToHandle fd
    queue <- newTQueueIO
    tid <- forkIO $ loop queue h 0
    forkIO $ redraw chan queue
    vty <- V.mkVty V.defaultConfig

    Signals.installHandler Signals.sigTERM (Signals.Catch $ handleShutdown vty tid fd) Nothing

    void $ customMain (pure vty) (Just chan) app c
    putStrLn "Bye!"
    killThread tid
    IO.hClose h

  else
    putStrLn "Given file descriptor doesn't exist"

steps :: CanvasState -> [Point] -> CanvasState
steps c@CanvasState{..} ps =
  let
    pointsToTuples = V.fromList $ map (\(Point x y) -> (x,y)) $ ps
    points' = V.concat [points, pointsToTuples]
    xMin' = V.foldl (\acc (x,_) -> min acc (prettyBounds x)) xMin pointsToTuples
    xMax' = V.foldl (\acc (x,_) -> max acc (prettyBounds x)) xMax pointsToTuples
    yMin' = V.foldl (\acc (_,y) -> min acc (prettyBounds y)) yMin pointsToTuples
    yMax' = V.foldl (\acc (_,y) -> max acc (prettyBounds y)) yMax pointsToTuples
  in
    c
    { points = points'
    , mergedPoints = points'
    , xMin = xMin'
    , xMax = xMax'
    , yMin = yMin'
    , yMax = yMax'
    }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, points = V.empty, mergedPoints = V.empty, width = w*brailleWidth, height = h*brailleHeight, xMin = 0.0, xMax = 0.0, yMin = 0.0, yMax = 0.0 }

app :: App CanvasState UiEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: CanvasState -> BrickEvent Name UiEvent -> EventM Name (Next CanvasState )
handleEvent c (AppEvent (Redraw ps)) = continue $ steps c ps
handleEvent c (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt c
handleEvent c (VtyEvent (V.EvKey V.KEsc []))        = halt c
handleEvent c (VtyEvent (V.EvResize w h))           = continue $ resize (w-2) (h-2) c
handleEvent c _                                     = continue c

drawUI :: CanvasState -> [Widget Name]
drawUI c =
  [
    hBox
      [
        vBox []
      , vBox [canvasWidget c, stats c ]
      ]
  ]

renderCanvas cs = setDots cs

stats c = padLeftRight 1 (str $ "Width: " ++ (show $ width c)) <+> padLeftRight 1 (str $ "Height: " ++ (show $ height c)) <+> padLeftRight 1 (str $ "X Min: " ++ (show $ xMin c)) <+> padLeftRight 1 (str $ "X Max: " ++ (show $ xMax c)) <+> padLeftRight 1 (str $ "Y Min: " ++ (show $ yMin c)) <+> padLeftRight 1 (str $ "Y Max: " ++ (show $ yMax c))

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