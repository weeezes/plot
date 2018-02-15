{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Tests.Braille
import {-@ HTF_TESTS @-} Tests.PointParser.Parser

main :: IO ()
main = htfMain htf_importedTests
