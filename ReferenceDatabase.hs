{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
 {-# LANGUAGE OverloadedStrings #-}


module ReferenceDatabase where
import VersionedFileFormat
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
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Applicative
import Control.Monad

rdTest :: IO ()
rdTest = do
	s <- readFile "jsontest.rdb" 
	let obj = (decode $ L.pack s) :: Maybe DBEntry
	case obj of 
		Just jobj -> putStrLn $ show obj
		Nothing -> putStrLn "failed to parse"

wdTest = do
	ltxString <- getLine 
	let js = encodePretty $ (DBEntry "0" [] [] True ltxString "" "" "" "")
	writeFile "jsontest.rdb" (L.unpack js)

-- overall database entry unifing biblography data, and reading metadata
data DBEntry = DBEntry {
	refID :: String, -- a bibtex ID for bibliographic referencing & indexing
	inCitations :: [String], -- List of bibtex IDs of inward citations/references
	outCitations :: [String], -- List of bibtex IDs of outward citations/references
	alreadyRead :: Bool, -- have I read the paper?
	--any notes about the paper, in LaTeX format
	generalNotes :: String,
	context :: String,
	keyContribution :: String,
	criticalReflection :: String,
	impact :: String
} deriving (Show)

instance ToJSON DBEntry where
	toJSON DBEntry{..} = object [ "refID" .= refID
								, "inCitations" .= inCitations
								, "outCitations" .= outCitations
								, "alreadyRead" .= alreadyRead
								, "generalNotes" .= generalNotes
								, "context" .= context
								, "keyContribution" .= keyContribution
								, "criticalReflection" .= criticalReflection
								, "impact" .= impact
								]

instance FromJSON DBEntry where
	parseJSON (Object v) = DBEntry <$>
							(v .: "refID") <*>
							(v .: "inCitations") <*>
							(v .: "outCitations") <*>
							(v .: "alreadyRead") <*>
							(v .: "generalNotes") <*>
							(v .: "context") <*>
							(v .: "keyContribution") <*>
							(v .: "criticalReflection") <*>
							(v .: "impact")
	parseJSON _ = mzero


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
