{-# LANGUAGE OverloadedStrings #-}
module ProcessDiff where

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

c = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "package.yaml", fileDeltaDestFile = "package.yaml", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 11, rangeNumberOfLines = 1}, hunkDestRange = Range {rangeStartingLineNumber = 10,
rangeNumberOfLines = 0}, hunkLines = [Line {lineAnnotation = Removed, lineContent = "- ChangeLog.md"}]}]}]

test2 = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asdf.cpp", fileDeltaDestFile = "asdf.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 2, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"}]},Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 4, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 6, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asdfC.cpp", fileDeltaDestFile = "asdfC.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines =
0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 14}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent =
"    char* lol;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    strcpy(lol, \"42\");"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]

test3 = Right [FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asqwe.h", fileDeltaDestFile = "asqwe.h", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber
= 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added,
lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "nope.hs", fileDeltaDestFile
= "nope.hs", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]
