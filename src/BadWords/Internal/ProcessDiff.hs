{-# LANGUAGE OverloadedStrings #-}
module BadWords.Internal.ProcessDiff
        (isRequiredFile
        ,onlyRequired
        ,suffixes
        ,badwords
        ,hasBadWords
        ,onlyBadWordDiffs
        ,wtf
        ) where

import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import Data.Functor
import qualified Data.Text as T

suffixes = [".h", ".cpp"]
badwords = ["strcpy", "sprintf"]


isRequiredFile :: FileDelta -> Bool
isRequiredFile (FileDelta _ src dst _) = or $ T.isSuffixOf <$> suffixes <*> filenames
    where filenames = [src, dst]


onlyRequired :: [FileDelta] -> [FileDelta]
onlyRequired = mfilter isRequiredFile


hasBadWords :: FileDelta -> Bool
hasBadWords (FileDelta _ _ _ (Hunks hunks)) = or $ T.isInfixOf <$> badwords <*> diffLines
    where diffLines = lineContent <$> (hunks >>= hunkLines)
hasBadWords (FileDelta _ _ _ Binary) = False


onlyBadWordDiffs :: [FileDelta] -> [FileDelta]
onlyBadWordDiffs = mfilter hasBadWords


-- testing
diffproc :: CreateProcess
diffproc = shell "git -C ~/git/testing-badwords diff HEAD^ HEAD -U0"

wtf :: IO ()
wtf = do
    b <- readCreateProcess diffproc ""
    putStr b
    putStrLn (map (const '=') [1..100])
    print $ parseDiff $ T.pack b
