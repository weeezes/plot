{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PointParser.Lazy
  ( loop
  ) where

import qualified System.IO as IO

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

import qualified Data.Vector as V
import qualified Data.Vector as VU
import Data.Time.Clock.POSIX
import Data.Time.Clock

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM(..), atomically)
import Control.Monad (foldM)

import Types
import PointParser.Parser

loop queue h = do
  isOpen <- IO.hIsOpen h
  if isOpen then do
    available <- BS.hGetContents h
    startTime <- getPOSIXTime
    loop' queue available  0
    endTime <- getPOSIXTime
    IO.hClose h
  else
    return ()

loop' queue availableData currentIndex = do
  if not (BS.null availableData) then do
    let r = AL.parse parsePoint availableData
    case r of
      AL.Fail i _ e -> print "Failed parsing"
      AL.Done leftover r -> do
        currentIndex' <- foldPoints queue currentIndex [r]
        loop' queue leftover currentIndex'
  else
    return ()