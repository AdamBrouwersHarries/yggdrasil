module ReferenceDatabase where
import Text.EditDistance
import Text.CSL.Reference

data RDBEntry = RDBEntry {
	id :: Int,
	refInfo :: Reference
}

type RefDB = [RDBEntry] 

emptyDB :: RefDB
emptyDB = []




matchReferences :: Reference -> Reference -> Float -> Bool
matchReferences r1 r2 t | r1 == r2 = True
						| otherwise = approxStringMatch (title r1) (title r2) >= t


approxStringMatch :: String -> String -> Float
approxStringMatch sa sb = (bigger-ld)/bigger where
	ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 
	bigger = fromIntegral $ max (length sa) (length sb)