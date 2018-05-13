{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TestGenerateHTML
    (htf_thisModulesTests
    ,prop_definitionSplitEntire1
    ) where

import BadWords.Internal.GenerateHTML
import Test.Framework
import Data.List
import qualified Data.Text as T

instance Arbitrary T.Text where
    -- arbitrary :: Gen T.Text
    arbitrary = fmap T.pack arbitrary

-- the concatenation of the resulting list should be the original word
prop_definitionSplitEntire1 :: [T.Text] -> T.Text -> Bool
prop_definitionSplitEntire1 words xs = xs == (T.concat split)
    where split = splitEntire words xs
