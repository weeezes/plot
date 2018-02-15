{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Tests.Braille

main :: IO ()
main = htfMain htf_importedTests
