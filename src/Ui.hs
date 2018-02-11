{-# LANGUAGE OverloadedStrings #-}

module Ui 
  ( runUi
  ) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Types (Size)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Widgets.Center as C

import Text.Wrap (WrapSettings(..))
import Lens.Micro ((^.))

import qualified Graphics.Vty as Vty

import qualified Options.Applicative as Options

import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Time.Clock.POSIX (getPOSIXTime)

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

import Types
import Braille
import PointParser.Lazy as PPL
import PointParser.Strict as PPS
import CanvasState

type Name = ()

untilNoneTimeout startTime acc queue = do
  currentTime <- getPOSIXTime
  if currentTime - startTime > 1 then do
    return $ V.fromList $ toList acc
  else do
    v <- atomically $ tryReadTQueue queue
    case v of
      Just v -> untilNoneTimeout startTime (acc Seq.>< Seq.fromList v) queue
      Nothing -> untilNoneTimeout startTime acc queue

redraw h shouldQuitAfterDone chan queue = do
  atomically $ peekTQueue queue -- Wait for first value before jumping into forever
  forever $ do
    startTime <- getPOSIXTime
    ps <- untilNoneTimeout startTime Seq.empty queue
    if V.length ps > 0 then do
      writeBChan chan $ Redraw ps
    else do
      isClosed <- IO.hIsClosed h
      noData <- atomically $ isEmptyTQueue queue
      if shouldQuitAfterDone && noData && isClosed then do
        writeBChan chan Die
      else
        return ()
      return ()
    threadDelay 100000
  return ()

fileInput :: Options.Parser (Maybe String)
fileInput = Options.optional $ Options.strOption
  (  Options.long "file"
  <> Options.short 'f'
  <> Options.metavar "FILENAME"
  <> Options.help "Input file" )

settingsParser :: Options.Parser Settings
settingsParser = Settings
  <$> Options.switch (Options.long "quit-after-done" <> Options.short 'k' <> Options.help "Quit instantly after done reading and drawing data")
  <*> Options.switch (Options.long "slurp" <> Options.short 's' <> Options.help "Read the full input to memory before parsing")
  <*> fileInput

settingsParserInfo :: Options.ParserInfo Settings
settingsParserInfo = Options.info (settingsParser Options.<**> Options.helper) 
                                  (Options.progDesc "Plot stuff on the terminal.")

handleShutdown :: Vty.Vty -> [ThreadId] -> IO.Handle -> IO ()
handleShutdown vty tids h = do
  putStrLn "Received SIGTERM, shutting down."
  -- TODO exit with proper error code if any of these fail
  mapM killThread tids
  IO.hClose h
  Vty.shutdown vty
  Process.exitImmediately Exit.ExitSuccess

runUi :: IO()
runUi = do
  chan <- newBChan 1
  let c = initCanvasState 50 20 :: CanvasState
  let w = width c
  let h = height c
  settings <- Options.execParser settingsParserInfo
  h <- case inputFd settings of
         Nothing -> return $ Just IO.stdin
         Just f  -> do
           exists <- D.doesFileExist f
           if exists then do
             h <- IO.openFile f IO.ReadMode
             return $ Just h
           else
             return Nothing

  case h of
    Just h -> do
      queue <- newTQueueIO
      loopTid <- if slurp settings then
                   forkIO $ PPS.loop queue h
                 else
                   forkIO $ PPL.loop queue h
      redrawTid <- forkIO $ redraw h (quitAfterDone settings) chan queue

      vty <- case inputFd settings of
               Nothing -> Vty.mkVty (Vty.defaultConfig { Vty.inputFd = Just PIO.stdError })
               Just _  -> Vty.mkVty Vty.defaultConfig

      Signals.installHandler Signals.sigTERM (Signals.Catch $ handleShutdown vty [loopTid, redrawTid] h) Nothing

      void $ customMain (pure vty) (Just chan) app c
      putStrLn "Bye!"
      mapM killThread [loopTid, redrawTid]
      IO.hClose h
    Nothing ->
      putStrLn "Given file descriptor doesn't exist"

app :: App CanvasState UiEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: CanvasState -> BrickEvent Name UiEvent -> EventM Name (Next CanvasState )
handleEvent c (AppEvent (Redraw ps))                = continue $ steps c ps
handleEvent c (AppEvent Die)                        = halt c
handleEvent c (VtyEvent (Vty.EvKey (Vty.KChar 'a') [])) = continue $ c { plotType = AreaPlot }
handleEvent c (VtyEvent (Vty.EvKey (Vty.KChar 'b') [])) = continue $ c { plotType = BarPlot }
handleEvent c (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = continue $ c { plotType = HistogramPlot }
handleEvent c (VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) = continue $ c { plotType = PointPlot }
handleEvent c (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt c
handleEvent c (VtyEvent (Vty.EvKey Vty.KEsc []))        = halt c
handleEvent c (VtyEvent (Vty.EvResize w h))           = continue $ resize (w-2) (h-2) c
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

stats c = padLeftRight 1 (str $ "Width: " ++ (show $ width c)) <+> padLeftRight 1 (str $ "Height: " ++ (show $ height c)) <+> padLeftRight 1 (str $ "X Min: " ++ (show $ xMin c)) <+> padLeftRight 1 (str $ "X Max: " ++ (show $ xMax c)) <+> padLeftRight 1 (str $ "Y Min: " ++ (show $ yMin c)) <+> padLeftRight 1 (str $ "Y Max: " ++ (show $ yMax c))

wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
canvasWidget :: CanvasState -> Widget n
canvasWidget cs =
  Widget Greedy Greedy $ do
      ctx <- getContext

      let width' = ctx^.availWidthL - 2
      let height' = ctx^.availHeightL - 2
      let c = plot $ cs { canvas = initCanvas width' height', width = width'*brailleWidth, height = height'*brailleHeight }
      
      render $ C.center $ withBorderStyle Border.unicodeBold
             $ B.borderWithLabel (str "Plot")
             $ vBox (rows width' height' (V.toList $ c))
  where
    rows w h c    = [strWrapWith wrapSettings $ map chr c]

theMap :: AttrMap
theMap = attrMap Vty.defAttr []