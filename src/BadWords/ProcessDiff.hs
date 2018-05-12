{-# LANGUAGE OverloadedStrings #-}
module BadWords.ProcessDiff
    (badWordsFromDiff
    ,filterDiffBySuffixes
    ) where

import BadWords.Internal.ProcessDiff
