{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TestProcessDiff
    (htf_thisModulesTests
    ) where

import Data.Either
import Data.List
import BadWords.Internal.ProcessDiff
import Test.Framework
import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import qualified Data.Text as T

-- file extensions filtration tests
test_filterNocpp = assertEqual result expected
    where result   = (fmap onlyRequired nocpp) :: Either String FileDeltas
          expected = Right []

test_filterAllcpp = assertEqual result expected
    where result   = (fmap onlyRequired allcpp) :: Either String FileDeltas
          expected = allcpp

test_filterSomecpp = assertNotEqual result expected
    where result   = (fmap onlyRequired somecpp) :: Either String FileDeltas
          expected = somecpp


-- bad word filtration tests
test_filterNobad = assertEqual result expected
    where result   = (fmap onlyBadWordDiffs nocpp) :: Either String FileDeltas
          expected = Right []

test_filterIntroducebad1 = (assertElem "asdf.cpp"  listOfFilteredNames) *>
                          (assertElem "asdfC.cpp" listOfFilteredNames)
    where filtered            = (fmap (onlyBadWordDiffs . onlyRequired) allcpp) :: Either String FileDeltas
          listOfFilteredNames = nub $ fmap fileDeltaSourceFile (fromRight [] filtered) ++
                                      fmap fileDeltaDestFile   (fromRight [] filtered)

test_filterIntroducebad2 = (assertElem "asqwe.h"  listOfFilteredNames)
    where filtered            = (fmap (onlyBadWordDiffs . onlyRequired) somecpp) :: Either String FileDeltas
          listOfFilteredNames = nub $ fmap fileDeltaSourceFile (fromRight [] filtered) ++
                                      fmap fileDeltaDestFile   (fromRight [] filtered)

-- test for introduction in bad words only in some files
test_filterIntroducebad3 = (assertElem "asdf.cpp"  listOfFilteredNames) *>
                           (assertBool $ not $ "asqwe.h" `elem` listOfFilteredNames)
    where filtered            = (fmap (onlyBadWordDiffs . onlyRequired) somebad) :: Either String FileDeltas
          listOfFilteredNames = nub $ fmap fileDeltaSourceFile (fromRight [] filtered) ++
                                      fmap fileDeltaDestFile   (fromRight [] filtered)


nocpp = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "package.yaml", fileDeltaDestFile = "package.yaml", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 11, rangeNumberOfLines = 1}, hunkDestRange = Range {rangeStartingLineNumber = 10,
rangeNumberOfLines = 0}, hunkLines = [Line {lineAnnotation = Removed, lineContent = "- ChangeLog.md"}]}]}]

-- allcpp diff
-- diff --git a/asdf.cpp b/asdf.cpp
-- index 66db97e..f4551dc 100644
-- --- a/asdf.cpp
-- +++ b/asdf.cpp
-- @@ -1,7 +1,9 @@
--  #include <iostream>
-- +#include <neshtosi>
-- 
--  int main()
--  {
-- +    sprintf("%%%ASDQ!@#", lol);
--      using std::cout;
--      using std::endl;
-- 
-- diff --git a/asdfC.cpp b/asdfC.cpp
-- new file mode 100644
-- index 0000000..77c4c54
-- --- /dev/null
-- +++ b/asdfC.cpp
-- @@ -0,0 +1,14 @@
-- +#include <iostream>
-- +
-- +int main()
-- +{
-- +    using std::cout;
-- +    using std::endl;
-- +
-- +    cout<<"lol"<<endl;
-- +
-- +    char* lol;
-- +
-- +    strcpy(lol, "42");
-- +    return 0;
-- +}
allcpp = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asdf.cpp", fileDeltaDestFile = "asdf.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 2, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"}]},Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 4, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 6, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asdfC.cpp", fileDeltaDestFile = "asdfC.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines =
0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 14}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent =
"    char* lol;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    strcpy(lol, \"42\");"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]


-- somecpp diff
-- diff --git a/asqwe.h b/asqwe.h
-- new file mode 100644
-- index 0000000..f4551dc
-- --- /dev/null
-- +++ b/asqwe.h
-- @@ -0,0 +1,12 @@
-- +#include <iostream>
-- +#include <neshtosi>
-- +
-- +int main()
-- +{
-- +    sprintf("%%%ASDQ!@#", lol);
-- +    using std::cout;
-- +    using std::endl;
-- +
-- +    cout<<"lol"<<endl;
-- +    return 0;
-- +}
-- diff --git a/nope.hs b/nope.hs
-- new file mode 100644
-- index 0000000..f4551dc
-- --- /dev/null
-- +++ b/nope.hs
-- @@ -0,0 +1,12 @@
-- +#include <iostream>
-- +#include <neshtosi>
-- +
-- +int main()
-- +{
-- +    sprintf("%%%ASDQ!@#", lol);
-- +    using std::cout;
-- +    using std::endl;
-- +
-- +    cout<<"lol"<<endl;
-- +    return 0;
-- +}
somecpp = Right [FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "asqwe.h", fileDeltaDestFile = "asqwe.h", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber
= 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added,
lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]},FileDelta {fileDeltaStatus = Created, fileDeltaSourceFile = "nope.hs", fileDeltaDestFile
= "nope.hs", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 0, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 1, rangeNumberOfLines = 12}, hunkLines = [Line {lineAnnotation = Added, lineContent = "#include <iostream>"},Line {lineAnnotation = Added, lineContent = "#include <neshtosi>"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "int main()"},Line {lineAnnotation = Added, lineContent = "{"},Line {lineAnnotation = Added, lineContent = "    sprintf(\"%%%ASDQ!@#\", lol);"},Line {lineAnnotation = Added, lineContent = "    using std::cout;"},Line {lineAnnotation = Added, lineContent = "    using std::endl;"},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    cout<<\"lol\"<<endl;"},Line {lineAnnotation = Added, lineContent = "    return 0;"},Line {lineAnnotation = Added, lineContent = "}"}]}]}]

-- introduce bad only in some existing
-- diff --git a/asdf.cpp b/asdf.cpp
-- index f4551dc..98e56f6 100644
-- --- a/asdf.cpp
-- +++ b/asdf.cpp
-- @@ -10,0 +11,5 @@ int main()
-- +
-- +    strcpy("kek");
-- +
-- +
-- +    more strcpy;
-- diff --git a/asqwe.h b/asqwe.h
-- index f4551dc..7649a31 100644
-- --- a/asqwe.h
-- +++ b/asqwe.h
-- @@ -5,0 +6 @@ int main()
-- +    nothing bad here;
-- @@ -6,0 +8 @@ int main()
-- +    nothing bad here;
-- @@ -10,0 +13 @@ int main()
-- +
somebad = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asdf.cpp", fileDeltaDestFile = "asdf.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 10, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 11, rangeNumberOfLines = 5}, hunkLines = [Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    strcpy(\"kek\");"},Line {lineAnnotation = Added, lineContent =
""},Line {lineAnnotation = Added, lineContent = ""},Line {lineAnnotation = Added, lineContent = "    more strcpy;"}]}]},FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asqwe.h", fileDeltaDestFile = "asqwe.h", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 5, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 6, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "    nothing bad here;"}]},Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 6, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 8, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = "    nothing bad here;"}]},Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 10, rangeNumberOfLines = 0}, hunkDestRange = Range {rangeStartingLineNumber = 13, rangeNumberOfLines = 1}, hunkLines = [Line {lineAnnotation = Added, lineContent = ""}]}]}]