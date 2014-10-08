{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ReferenceDatabase where

import TH (litFile)
import Text.EditDistance
import Text.CSL.Reference
import Text.CSL.Style
import Text.CSL.Parser
import Text.CSL.Pickle
import Text.CSL
import Text.CSL.Input.Identifier.Internal as Internal
import Text.CSL.Input.Identifier
import Text.CSL.Reference
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString




-- overall database entry unifing biblography data, and reading metadata
data RDBEntry = RDBEntry {
	dbID :: Int, -- a database ID for fast matching when searching for references
	refID :: String, -- a bibtex ID for bibliographic referencing
	bibEntry :: Reference -- a bibtex entry for full paper data
}

-- and a type for holding the database
type RefDB = [RDBEntry] 

data BibMetadata = BibMetadata {
	inCitations :: [RDBEntry],
	outCitations :: [RDBEntry],
	alreadyRead :: Bool,
	notes :: String
}

emptyDB :: RefDB
emptyDB = []

paperName :: RDBEntry -> String
paperName r = title $ bibEntry r

matchReferences :: Reference -> Reference -> Float -> Bool
matchReferences r1 r2 t | r1 == r2 = True
						| otherwise = approxStringMatch (title r1) (title r2) >= t


approxStringMatch :: String -> String -> Float
approxStringMatch sa sb = (bigger-ld)/bigger where
	ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 
	bigger = fromIntegral $ max (length sa) (length sb)

formatAsBibtex :: Reference -> String
formatAsBibtex r = renderPlainStrict $ head $ processBibliography procOpts bibtexCSLDef [r]

bibtexCSLDef :: Style
bibtexCSLDef = readXmlString xpStyle $ L.pack $ show $[litFile|bibtex.csl|]
