{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} TestProcessDiff
import {-@ HTF_TESTS @-} TestGenerateHTML

main :: IO ()
main = htfMain htf_importedTests
