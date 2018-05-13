{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TestProcessDiff
    (htf_thisModulesTests
    ) where
-- Abandon All Hope, Ye Who Enter Here

import Data.Either
import Data.List
import BadWords.Internal.ProcessDiff
import BadWords.Types
import Test.Framework
import Text.Diff.Parse
import Text.Diff.Parse.Types
import System.Process
import Control.Monad
import qualified Data.Text as T

-- test consts
suff :: [T.Text]
suff = [".h", ".cpp"]
key :: [T.Text]
key = ["strcpy", "sprintf"]

-- file extensions filtration tests
test_filterNocpp = assertEqual result expected
    where result   = (fmap (filterDiffBySuffixes suff) nocpp) :: Either String FileDeltas
          expected = Right []

test_filterAllcpp = assertEqual result expected
    where result   = (fmap (filterDiffBySuffixes suff) allcpp) :: Either String FileDeltas
          expected = allcpp

test_filterSomecpp = assertNotEqual result expected
    where result   = (fmap (filterDiffBySuffixes suff) somecpp) :: Either String FileDeltas
          expected = somecpp


-- bad words + lines test
test_badWordsNocpp = assertEqual expected result
    where result   = ((fmap (badWordsFromDiff key) nocpp) :: Either String [BadWords])
          expected =  (Right [])

test_badWordsintroduced1 = assertEqual expected result
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) allcpp) :: Either String [BadWords]) 
          expected = Right $ [(BadWords "asdf.cpp" [(6, Line Added "    sprintf(\"%%%ASDQ!@#\", lol);")]),
                              (BadWords "asdfC.cpp" [(12 , Line Added "    strcpy(lol, \"42\");")])]

test_badWordsintroduced2 = assertEqual expected result
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) somebad) :: Either String [BadWords]) 
          expected = Right $ [BadWords "asdf.cpp" [(12, Line Added "    strcpy(\"kek\");"),
                                                   (15, Line Added "    more strcpy;")]]

test_badWordsremoved1 = assertEqual expected result
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) removebad) :: Either String [BadWords]) 
          expected = Right $ [BadWords "asdfC.cpp" [(12, Line Removed "    strcpy(lol, \"42\");")]]


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

-- remove one bad
-- diff --git a/asdfC.cpp b/asdfC.cpp
-- index 77c4c54..4bc9e3b 100644
-- --- a/asdfC.cpp
-- +++ b/asdfC.cpp
-- @@ -9,6 +9,5 @@ int main()
-- 
--      char* lol;
-- 
-- -    strcpy(lol, "42");
--      return 0;
--  }
removebad = Right [FileDelta {fileDeltaStatus = Modified, fileDeltaSourceFile = "asdfC.cpp", fileDeltaDestFile = "asdfC.cpp", fileDeltaContent = Hunks [Hunk {hunkSourceRange = Range {rangeStartingLineNumber = 9, rangeNumberOfLines = 6}, hunkDestRange = Range {rangeStartingLineNumber = 9, rangeNumberOfLines = 5}, hunkLines = [Line {lineAnnotation = Context, lineContent = ""},Line {lineAnnotation = Context, lineContent = "    char* lol;"},Line {lineAnnotation = Context, lineContent = ""},Line {lineAnnotation = Removed, lineContent = "    strcpy(lol, \"42\");"},Line {lineAnnotation = Context, lineContent = "    return 0;"},Line {lineAnnotation = Context, lineContent = "}"}]}]}]
