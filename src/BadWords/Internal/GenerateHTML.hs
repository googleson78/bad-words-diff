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

redSpan_   = span_ [style_ "color:red"] 
greenSpan_ = span_ [style_ "color:green"] 
greySpan_  = span_ [style_ "color:grey"] 
blackSpan_ = span_ [style_ "color:black"] 

annoToColouredSym :: Annotation -> Html ()
annoToColouredSym Added   = greenSpan_ $ toHtml "+"
annoToColouredSym Removed = redSpan_   $ toHtml "-"
annoToColouredSym Context = toHtml " "

colourBad :: T.Text -> T.Text -> Html ()
colourBad bad word 
    | word == bad = redSpan_ $ toHtml $ word
    | otherwise   = toHtml word


colourBadWord :: T.Text -> (LineNum, Line) -> Html ()
colourBadWord badword (n, (Line anno text)) = formatted
    where split = splitEntire badword text
          coloured = foldMap (colourBad badword) split
          colouredAnno = annoToColouredSym anno
          lineNum = greySpan_ $ toHtml $ T.pack $ show $ n
          formatted = lineNum <> colouredAnno <> coloured
