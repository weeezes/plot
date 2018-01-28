{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PointParser
  ( loop
  ) where

import qualified System.IO as IO

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

import qualified Data.Vector as V
import qualified Data.Vector as VU
import Data.Time.Clock.POSIX
import Data.Time.Clock

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM(..), atomically)

import Types

data ParsedPoint = ParsedPoint Point | ParsedSingle Double deriving (Show)
data Perf = Perf
  { lastTime :: POSIXTime
  , currentTime :: POSIXTime
  , lastCount :: Int
  , currentCount :: Int
  }

logPerf perf@Perf{..} = do
  let tdiff = realToFrac $ currentTime - lastTime
  let cdiff = currentCount - lastCount
  if tdiff /= 0 then do
    let p = (fromIntegral cdiff) / tdiff
    BSC.appendFile "./log" $ BSC.concat ["Perf: ", BSC.pack (show p) , "\n"]
  else
    return ()

parsePoint :: A.Parser ParsedPoint
parsePoint = do
  x <- A.signed A.double
  A.takeWhile (\c -> not (A.isDigit c || c == '.' || c == '-' || c == '\n' || c == '\r'))
  possiblyDigit <- A.peekChar
  p <- case possiblyDigit of
         Just c -> 
           if A.isDigit c || c == '-' then do
             y <- A.signed A.double
             return $ ParsedPoint (x,y)
           else
             return $ ParsedSingle x
         Nothing -> return $ ParsedSingle x
  A.takeTill (\c -> c == '\r' || c == '\n')
  A.endOfLine
  return p

parsedPointToPoint :: Int -> Int -> ParsedPoint -> Point
parsedPointToPoint currentIndex _ (ParsedPoint p) = p
parsedPointToPoint currentIndex x (ParsedSingle y) = (fromIntegral $ currentIndex + x,y)

foldPoints queue acc ps = do
  atomically $ writeTQueue queue $ parsedPointToPoint acc acc ps
  return $ acc + 1

loop queue h = do
  isOpen <- IO.hIsOpen h
  if isOpen then do
    available <- BS.hGetContents h
    startTime <- getPOSIXTime
    let r = AL.parse (AL.many1 parsePoint) available
    case r of
      AL.Fail i _ e -> print "Failed parsing"
      AL.Done i r -> do
        loop' queue (Perf { lastTime = startTime, currentTime = startTime, lastCount = 0, currentCount = 0 }) 0 r
    IO.hClose h
  else
    return ()

loop' queue perf currentIndex points = do
  if not (null points) then do
    currentIndex' <- foldPoints queue currentIndex (head points)
    loop' queue perf currentIndex' (tail points)
  else
    return ()
 