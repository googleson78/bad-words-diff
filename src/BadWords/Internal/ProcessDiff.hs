{-# LANGUAGE OverloadedStrings #-}
module BadWords.Internal.ProcessDiff
        (isRequiredFile
        ,onlyRequired
        ,suffixes
        ,keywords
        ,badWordsFromDiff
        ,BadWords(..)
        ,wtf
        ) where

import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import Data.Functor()
import Data.Maybe
import BadWords.Types
import qualified Data.Text as T

suffixes :: [T.Text]
suffixes = [".h", ".cpp"]
keywords :: [T.Text]
keywords = ["strcpy", "sprintf"]


isRequiredFile :: FileDelta -> Bool
isRequiredFile (FileDelta _ src dst _) = or $ T.isSuffixOf <$> suffixes <*> filenames
    where filenames = [src, dst]

onlyRequired :: [FileDelta] -> [FileDelta]
onlyRequired = mfilter isRequiredFile

hasBadWord :: Line -> Bool
hasBadWord (Line _ text) = or $ fmap (T.isInfixOf `flip` text) keywords

tagListIx :: [a] -> [(Int, a)]
tagListIx = zip [0..]

taggedBadLines :: Hunk -> [(LineNum, Line)]
taggedBadLines (Hunk srcRng dstRng lns) = badLines
    where tagged   = tagListIx lns
          filtered = filter (hasBadWord . snd) tagged
          badLines = fmap offsetLineNum filtered

          offsetLineNum :: (LineNum, Line) -> (LineNum, Line)
          offsetLineNum (num, ln) = (modWithRange (lineRange ln) num, ln)

          lineRange :: Line -> Range
          lineRange (Line Removed _) = srcRng
          lineRange _                = dstRng

          modWithRange :: Range -> LineNum -> LineNum
          modWithRange (Range start _) = (+start)


badWordsFromDelta :: FileDelta -> Maybe BadWords
badWordsFromDelta (FileDelta _ _ _ Binary)            = Nothing
badWordsFromDelta (FileDelta stat src dst (Hunks hs)) = case badLines of
                                                         [] -> Nothing
                                                         lns -> Just $ BadWords name lns
    where badLines = hs >>= taggedBadLines
          name = whichFile stat

          whichFile :: FileStatus -> T.Text
          whichFile Deleted = src
          whichFile _       = dst

badWordsFromDiff :: [FileDelta] -> [BadWords]
badWordsFromDiff diff = diff >>= (maybeToList . badWordsFromDelta)

-- testing
diffproc :: CreateProcess
diffproc = shell "git -C ~/git/testing-badwords diff HEAD^ HEAD"

wtf :: IO ()
wtf = do
    b <- readCreateProcess diffproc ""
    putStr b
    putStrLn (map (const '=') [1..100])
    print $ parseDiff $ T.pack b
