{-# LANGUAGE OverloadedStrings #-}
module BadWords.Internal.ProcessDiff
        (isRequiredFile
        ,onlyRequired
        ) where

import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import qualified Data.Text as T

isRequiredFile :: FileDelta -> Bool
isRequiredFile (FileDelta _ src dst _) = or $ T.isSuffixOf <$> suffixes <*> filenames
    where suffixes = [".h", ".cpp"]
          filenames = [src, dst]

onlyRequired :: [FileDelta] -> [FileDelta]
onlyRequired = mfilter isRequiredFile



-- testing
diffproc :: CreateProcess
diffproc = shell "git -C ~/git/testing-badwords diff HEAD^ HEAD -U0"

wtf :: IO ()
wtf = do
    b <- readCreateProcess diffproc ""
    putStr b
    putStrLn (map (const '=') [1..100])
    print $ parseDiff $ T.pack b
