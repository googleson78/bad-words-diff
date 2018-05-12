{-# LANGUAGE OverloadedStrings #-}
module BadWords.Types
    (BadWords(..)
    ,LineNum
    ) where

import qualified Data.Text as T
import Text.Diff.Parse.Types

type LineNum = Int

data BadWords = BadWords {fileName :: T.Text,
                          lines    :: [(LineNum, Line)]}
    deriving (Eq, Show)
