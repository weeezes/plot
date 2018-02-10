{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PointParser.Strict
  ( loop
  ) where

import qualified System.IO as IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as A

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

    let r = A.parseOnly (A.many1 parsePoint) available
    case r of
      Left e -> print e
      Right r -> foldPoints queue 0 r >> return ()

    endTime <- getPOSIXTime
    IO.hClose h
  else
    return ()
