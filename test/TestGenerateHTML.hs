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

-- perhaps use Property here and increase number of tests
prop_definitionSplitBy :: T.Text -> T.Text -> Bool
prop_definitionSplitBy word xs = pref `T.append` maybeWord `T.append` suff == xs
    where (pref, suff) = splitBy word xs
          maybeWord    = if word `T.isInfixOf` xs then word else T.empty

prop_splitByKeepsPreffAndSuff :: T.Text -> T.Text -> Bool
prop_splitByKeepsPreffAndSuff word xs = pref `T.isPrefixOf` xs && suff `T.isSuffixOf` xs
    where (pref, suff) = splitBy word xs

-- prefix and suffix returned by splitBy are both null iff
-- word is an exact match of xs or xs is empty
prop_splitByPrefSuffNull :: T.Text -> T.Text -> Property
prop_splitByPrefSuffNull word xs = word == xs || xs == T.empty ==> 
                         pref == T.empty && suff == T.empty 
    where (pref, suff) = splitBy word xs

prop_splitByPrefSuffNull' :: T.Text -> T.Text -> Property
prop_splitByPrefSuffNull' word xs = pref == T.empty && suff == T.empty ==> 
                          word == xs || xs == T.empty
    where (pref, suff) = splitBy word xs

-- the concatenation of the resulting list should be the original word
prop_definitionSplitEntire1 :: T.Text -> T.Text -> Bool
prop_definitionSplitEntire1 word xs = xs == (T.concat split)
    where split = splitEntire word xs

-- foreach element of the resulting split (which is not the word itself)
-- it should be true that it does not contain word as a subword (infix)
prop_definitionSplitEntire2 :: T.Text -> T.Text -> Property
prop_definitionSplitEntire2 word xs = word /= "" && xs /= "" ==> 
                                      all (not . T.isInfixOf word) $ filter (/=word) split
    where split = splitEntire word xs
