{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TestGenerateHTML
    (htf_thisModulesTests
    ) where

import BadWords.Internal.GenerateHTML
import Test.Framework
import Data.List
import qualified Data.Text as T

instance Arbitrary T.Text where
    -- arbitrary :: Gen T.Text
    arbitrary = fmap T.pack arbitrary

prop_definitionSplitBy :: T.Text -> T.Text -> Bool
prop_definitionSplitBy inn xs = pref `T.append` maybeInn `T.append` suff == xs
    where (pref, suff) = splitBy inn xs
          maybeInn = if inn `T.isInfixOf` xs then inn else T.empty
