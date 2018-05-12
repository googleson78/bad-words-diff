-- test consts
suff :: [T.Text]
suff = [".h", ".cpp"]
key :: [T.Text]
key = ["strcpy", "sprintf"]

    where result   = (fmap (filterDiffBySuffixes suff) nocpp) :: Either String FileDeltas
    where result   = (fmap (filterDiffBySuffixes suff) allcpp) :: Either String FileDeltas
    where result   = (fmap (filterDiffBySuffixes suff) somecpp) :: Either String FileDeltas
    where result   = ((fmap (badWordsFromDiff key) nocpp) :: Either String [BadWords])
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) allcpp) :: Either String [BadWords]) 
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) somebad) :: Either String [BadWords]) 
    where result   = ((fmap ((badWordsFromDiff key) . (filterDiffBySuffixes suff)) removebad) :: Either String [BadWords]) 