{-# LANGUAGE OverloadedStrings #-}
module BadWords.Internal.ProcessDiff where

import Text.Diff.Parse.Types
import Control.Monad
import Data.Functor()
import Data.Maybe
import BadWords.Types
import qualified Data.Text as T
import qualified Data.List as L

isRequiredFile :: [T.Text] -> FileDelta -> Bool
isRequiredFile suffixes (FileDelta _ src dst _) = or $ T.isSuffixOf <$> suffixes <*> filenames
    where filenames = [src, dst]

filterDiffBySuffixes :: [T.Text] -> [FileDelta] -> [FileDelta]
filterDiffBySuffixes suffixes = mfilter $ isRequiredFile suffixes

hasBadWord :: [T.Text] -> Line -> Bool
hasBadWord keywords (Line _ text) = or $ fmap (T.isInfixOf `flip` text) keywords


tagListLines :: [Line] -> [(Int, Line)]
tagListLines = reverse . fst' . foldl fn ([], [0..], [0..], [0..]) 
    where fn :: ([(a, Line)], [a], [a], [a]) -> Line -> ([(a, Line)], [a], [a], [a])
          fn (lns, (x:xs), ys,     (_:zs)) ln@(Line Added   _) = ((x, ln) : lns, xs, ys, zs)
          fn (lns, xs,     (y:ys), (_:zs)) ln@(Line Removed _) = ((y, ln) : lns, xs, ys, zs)
          fn (lns, (_:xs), (_:ys), (z:zs)) ln@(Line Context _) = ((z, ln) : lns, xs, ys, zs)
          fst' (x, _, _, _) = x

taggedBadLines :: [T.Text] -> Hunk -> [(LineNum, Line)]
taggedBadLines keywords (Hunk srcRng dstRng lns) = badLines
    where tagged   = tagListLines lns
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
