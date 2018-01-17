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

import Control.Monad (forever, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)

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

parseSecondValue :: A.Parser Double
parseSecondValue = do
  A.skipSpace
  d <- A.double
  A.skipSpace
  A.option () A.endOfLine
  return $ d

parseSingles :: A.Parser [Double]
parseSingles = A.many' parseSecondValue

parsePoint :: A.Parser Point
parsePoint = do
  x <- A.double
  A.skipSpace
  y <- A.double
  A.skipSpace
  A.option () A.endOfLine
  return $ Point x y

loop chan h = do
  PP.evalStateT (PP.foldAllM (\_ p -> writeBChan chan p >>= \_ -> return ()) (return ()) (\_ -> return ())) $ PA.parsed parsePoint (PB.fromHandle h)
  return ()

loopSingles chan h x = do
  PP.evalStateT (PP.foldAllM (\x v -> writeBChan chan (Point x v) >>= \_ -> return $ x+1) (return 0 :: IO Double) pure) $ PA.parsed parseSecondValue (PB.fromHandle h)
  return ()

settingsParser :: Options.Parser Settings
settingsParser = Settings
  <$> Options.argument Options.str (Options.metavar "FD")
  <*> Options.switch (Options.long "singles" <> Options.short 's')

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
    if singles settings then do
      tid <- forkIO $ loopSingles chan h 0
      vty <- V.mkVty V.defaultConfig

      Signals.installHandler Signals.sigTERM (Signals.Catch $ handleShutdown vty tid fd) Nothing

      void $ customMain (pure vty) (Just chan) app c
      putStrLn "Bye!"
      killThread tid
      IO.hClose h

    else do
      tid <- forkIO $ loop chan h
      vty <- V.mkVty V.defaultConfig

      Signals.installHandler Signals.sigTERM (Signals.Catch $ handleShutdown vty tid fd) Nothing

      void $ customMain (pure vty) (Just chan) app c
      putStrLn "Bye!"
      killThread tid
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