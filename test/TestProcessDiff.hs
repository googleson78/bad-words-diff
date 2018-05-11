{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TestProcessDiff
    (htf_thisModulesTests
    ) where

import BadWords.Internal.ProcessDiff
import Test.Framework
import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import qualified Data.Text as T

test_filterNocpp = assertEqual ((fmap onlyRequired nocpp) :: Either String FileDeltas) (Right [])


nocpp = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "package.yaml", fileDeltaDestFile = "package.yaml", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 11, rangeNumberOfLines = 1}, hunkDestRange = Range {rangeStartingLineNumber = 10,
rangeNumberOfLines = 0}, hunkLines = [Line {lineAnnotation = Removed, lineContent = "- ChangeLog.md"}]}]}]

allcpp = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asdf.cpp", fileDeltaDestFile = "asdf.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 2, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"}]},Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 4, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 6, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asdfC.cpp", fileDeltaDestFile = "asdfC.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines =
0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 14}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent =
"    char* lol;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    strcpy(lol, \"42\");"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]

somecpp = Right [FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asqwe.h", fileDeltaDestFile = "asqwe.h", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber
= 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added,
lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "nope.hs", fileDeltaDestFile
= "nope.hs", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]
