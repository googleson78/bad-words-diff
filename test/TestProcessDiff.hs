-- Abandon All Hope, Ye Who Enter Here
import BadWords.Types
-- bad words + lines test
test_badWordsNocpp = assertEqual result expected
    where result   = ((fmap badWordsFromDiff nocpp) :: Either String [BadWords])
          expected =  (Right [])
test_badWordsintroduced1 = assertEqual result expected
    where result   = ((fmap (badWordsFromDiff . onlyRequired) allcpp) :: Either String [BadWords]) 
          expected = Right $ [(BadWords "asdf.cpp" [(6, Line Added "    sprintf(\"%%%ASDQ!@#\", lol);")]),
                              (BadWords "asdfC.cpp" [(12 , Line Added "    strcpy(lol, \"42\");")])]
test_badWordsintroduced2 = assertEqual result expected
    where result   = ((fmap (badWordsFromDiff . onlyRequired) somebad) :: Either String [BadWords]) 
          expected = Right $ [BadWords "asdf.cpp" [(12, Line Added "    strcpy(\"kek\");"),
                                                   (15, Line Added "    more strcpy;")]]
test_badWordsremoved1 = assertEqual result expected
    where result   = ((fmap (badWordsFromDiff . onlyRequired) removebad) :: Either String [BadWords]) 
          expected = Right $ [BadWords "asdfC.cpp" [(12, Line Removed "    strcpy(lol, \"42\");")]]

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