{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module BadWords.Internal.GenerateHTML where

--
import BadWords.Types
import Text.Diff.Parse.Types
import Lucid
import Lucid.Base
import Data.Monoid
import qualified Data.Text as T
import qualified Data.List as L
import Data.String
import Data.Maybe

-- perhaps handle empty words in a different way?
--
-- fromJust never throws, because matching is a prefix of l
-- in the case that we call it
splitEntire :: [T.Text] -> T.Text -> [T.Text]
splitEntire _          (T.uncons -> Nothing) = [""]
splitEntire keywords l@(T.uncons -> Just (x, xs)) 
    | L.any (T.isPrefixOf `flip` l) keywords' = "" : matching : splitEntire keywords' stripped
    | otherwise             = (T.cons x next) : rest
    where keywords' = filter (/= "") keywords
          stripped = fromJust $ T.stripPrefix matching l
          (next:rest) = splitEntire keywords' xs
          matching = fromJust $ L.find (T.isPrefixOf `flip` l) keywords'

-- help me
redSpan_ :: (Term [a] result, Lucid.Base.TermRaw arg a, Data.String.IsString arg) => result
redSpan_   = span_ [style_ "color:red"] 
greenSpan_ :: (Term [a] result, Lucid.Base.TermRaw arg a, Data.String.IsString arg) => result
greenSpan_ = span_ [style_ "color:green"] 
greySpan_ :: (Term [a] result, Lucid.Base.TermRaw arg a, Data.String.IsString arg) => result
greySpan_  = span_ [style_ "color:grey"] 
blackSpan_ :: (Term [a] result, Lucid.Base.TermRaw arg a, Data.String.IsString arg) => result
blackSpan_ = span_ [style_ "color:black"] 

annoToColouredSym :: Annotation -> Html ()
annoToColouredSym Added   = greenSpan_ $ toHtml ['+']
annoToColouredSym Removed = redSpan_   $ toHtml ['-']
annoToColouredSym Context = toHtml [' ']

colourBad :: [T.Text] -> T.Text -> Html ()
colourBad bad word 
    | word `elem` bad = redSpan_ $ toHtml $ word
    | otherwise   = toHtml word

formatLine :: [T.Text] -> (LineNum, Line) -> Html ()
formatLine badword (n, (Line anno text)) = formatted
    where split = splitEntire badword text
          coloured = foldMap (colourBad badword) split
          colouredAnno = annoToColouredSym anno
          lineNum :: Html () -- huh?
          lineNum = greySpan_ $ toHtml $ T.pack $ show $ n
          formatted = colouredAnno <> lineNum <> coloured <> br_ []

formatBadWords :: [T.Text] -> BadWords -> Html ()
formatBadWords keywords (BadWords file lns) = pre_ (p_ (b_ $ toHtml file) <> formatted)
    where formatted = foldMap (formatLine keywords) lns
