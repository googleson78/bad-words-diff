{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} TestProcessDiff
import {-@ HTF_TESTS @-} TestGenerateHTML

args = Args Nothing 500 50 7 True 9223372036854775807

main :: IO ()
main = setDefaultArgs args >> htfMain htf_importedTests
