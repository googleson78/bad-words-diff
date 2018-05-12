{-# LANGUAGE OverloadedStrings #-}
module BadWords.Internal.ProcessDiff
        (isRequiredFile
        ,filterDiffBySuffixes
        ,badWordsFromDiff
        ,BadWords(..)
        ) where

import Text.Diff.Parse.Types
import Control.Monad
import Data.Functor()
import Data.Maybe
import BadWords.Types
import qualified Data.Text as T

isRequiredFile :: [T.Text] -> FileDelta -> Bool
isRequiredFile suffixes (FileDelta _ src dst _) = or $ T.isSuffixOf <$> suffixes <*> filenames
    where filenames = [src, dst]

filterDiffBySuffixes :: [T.Text] -> [FileDelta] -> [FileDelta]
filterDiffBySuffixes suffixes = mfilter $ isRequiredFile suffixes

hasBadWord :: [T.Text] -> Line -> Bool
hasBadWord keywords (Line _ text) = or $ fmap (T.isInfixOf `flip` text) keywords


tagListIx :: [a] -> [(Int, a)]
tagListIx = zip [0..]

taggedBadLines :: [T.Text] -> Hunk -> [(LineNum, Line)]
taggedBadLines keywords (Hunk srcRng dstRng lns) = badLines
    where tagged   = tagListIx lns
          filtered = filter (hasBadWord keywords . snd) tagged
          badLines = fmap offsetLineNum filtered

          offsetLineNum :: (LineNum, Line) -> (LineNum, Line)
          offsetLineNum (num, ln) = (modWithRange (lineRange ln) num, ln)

          lineRange :: Line -> Range
          lineRange (Line Removed _) = srcRng
          lineRange _                = dstRng

          modWithRange :: Range -> LineNum -> LineNum
          modWithRange (Range start _) = (+start)


badWordsFromDelta :: [T.Text] -> FileDelta -> Maybe BadWords
badWordsFromDelta keywords (FileDelta _ _ _ Binary)            = Nothing
badWordsFromDelta keywords (FileDelta stat src dst (Hunks hs)) = case badLines of
                                                                  [] -> Nothing
                                                                  lns -> Just $ BadWords name lns
    where badLines = hs >>= taggedBadLines keywords
          name = whichFile stat

          whichFile :: FileStatus -> T.Text
          whichFile Deleted = src
          whichFile _       = dst

badWordsFromDiff :: [T.Text] -> [FileDelta] -> [BadWords]
badWordsFromDiff keywords diff = diff >>= (maybeToList . badWordsFromDelta keywords)
