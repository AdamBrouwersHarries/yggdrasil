module DB.Search where

import DB.PaperDB
import Text.EditDistance
import Data.List

sortTitles :: String -> [(Integer, String)] -> [(Integer, String)]
sortTitles t xs = map snd $ 
    sortBy (\a b -> compare (fst a) (fst b)) $ 
    map (\x -> (levenshteinDistance defaultEditCosts t (snd x), x)) xs

bothDLD :: String -> String -> (Int, Int)
bothDLD sa sb = (da, db) where
    da = levenshteinDistance defaultEditCosts sa sb
    db = levenshteinDistance defaultEditCosts sb sa

approxStringMatch :: Fractional a => String -> String -> a
approxStringMatch sa sb = (bigger-ld)/bigger where
    ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 
    bigger = fromIntegral $ max (length sa) (length sb)

approxStringMatch' :: Fractional t => String -> String -> (t, t, t)
approxStringMatch' sa sb = ( ((sal-ld)/sal),  ((sbl-ld)/sbl), ld) where
    sal = fromIntegral $ length sa
    sbl = fromIntegral $ length sb
    ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 

