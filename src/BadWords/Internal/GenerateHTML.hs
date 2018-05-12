{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module BadWords.Internal.GenerateHTML where

import BadWords.Types
import Text.Diff.Parse.Types
import Lucid
import Data.Monoid
import qualified Data.Text as T
import qualified Data.List as L
import Data.String
import Data.Maybe
    

-- fromJust never throws, because inner is a prefix of l
-- in the case that we call it
splitBy :: T.Text -> T.Text -> (T.Text, T.Text)
splitBy word   (T.uncons -> Nothing) = (T.empty, T.empty)
splitBy word l@(T.uncons -> Just (x, xs))
    | word `T.isPrefixOf` l = (T.empty, fromJust $ T.stripPrefix word l)
    | otherwise              = (x `T.cons` f, t)
        where (f, t) = splitBy word xs 

splitEntire :: T.Text -> T.Text -> [T.Text]
splitEntire (T.null -> True) text        = [text]
splitEntire word   (T.uncons -> Nothing) = [""]
splitEntire word l@(T.uncons -> Just (x, xs)) 
    | word `T.isPrefixOf` l = "" : word : splitEntire word stripped
    | otherwise             = (T.cons x next) : rest
    where stripped = fromJust $ T.stripPrefix word l
          (next:rest) = splitEntire word xs
