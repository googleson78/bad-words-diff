{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, ViewPatterns #-}
module BadWords.Internal.GenerateHTML where

import BadWords.Types
import Lucid
import Data.Monoid
import qualified Data.Text as T
import qualified Data.List as L
import Data.String
import Data.Maybe
    
splitBy :: T.Text -> T.Text -> (T.Text, T.Text)
splitBy inner   (T.uncons -> Nothing) = (T.empty, T.empty)
splitBy inner l@(T.uncons -> Just (x, xs))
    | inner `T.isPrefixOf` l = (T.empty, fromJust $ T.stripPrefix inner l)
--                                 fromJust never throws, because inner is a prefix of l
    | otherwise              = (x `T.cons` f, t)
        where (f, t) = splitBy inner xs 
