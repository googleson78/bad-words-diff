{-# LANGUAGE OverloadedStrings #-}
module Main where

import BadWords.ProcessDiff
import BadWords.GenerateHTML
import BadWords.Types
import System.Process
import System.Environment
import Text.Diff.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Data.Either
import Lucid

usage :: T.Text
usage = "usage: Pass 6+ arguments in the form: <path-to-git> <commit1> <commit2> <output-path> <list of +1 suffixes> <list of +1 bad words>. Suffixes *must* start with '.'"

-- suffixes are hardcoded for now
--suffixes :: [T.Text]
--suffixes = [".cpp", ".h"]
-- assume git is in PATH
main :: IO ()
main = do
    args <- getArgs
    if (length args < 5) 
    then 
        (T.putStrLn usage) 
    else do
        let gitPath  = args !! 0
        let commit1  = args !! 1
        let commit2  = args !! 2
        let outPath  = args !! 3
        let (suffixes, badWords) = (\(x, y) -> (T.pack <$> x, T.pack <$> y)) $ span ((=='.') . head) $ drop 4 args
        let command  = "git -C \"" ++ gitPath ++ "\" diff -U0 \"" ++ commit1 ++ "\" \"" ++ commit2 ++ "\""
        let proc     = shell command
        diff <- readCreateProcess proc ""
        let parsedDiff = parseDiff $ T.pack diff
        if not $ (isRight parsedDiff)
        then
            putStrLn "git diff or git diff parsing failed"
        else do
            let diff              = fromRight (error "isRight is buggy") parsedDiff
            let filteredFilesDiff = filterDiffBySuffixes suffixes diff
            let filteredBadWordsDiff = badWordsFromDiff badWords filteredFilesDiff
            let html = foldMap (formatBadWords badWords) filteredBadWordsDiff
            renderToFile outPath html
