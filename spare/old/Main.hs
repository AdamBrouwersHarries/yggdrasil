module Main where
import ReferenceDatabase
import Text.EditDistance
import Text.CSL
import Text.CSL.Input.Identifier.Internal as Internal
import Text.CSL.Input.Identifier
import Text.CSL.Reference
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Either 
import Control.Monad.State as State
import Data.Aeson

main :: IO ()
main = do
	wdTest
	rdTest
--main = do
--	m <- readBiblioFile "mybibdb.bib"
--	s <- readCSLFile "bibtex.csl"
--	--putStrLn $ unlines $ map (show) m
--	let result = processBibliography procOpts s m
--	let procAsBibtex = processBibliography procOpts s 
--	-- the following retains the order 
--	--let result = map (\r -> renderPlainStrict $ head $ procAsBibtex [r]) m

--	putStrLn $ foldl (\x y -> x++"\n\n\n\n\n"++y) "" $ map show $ (zip (map (show . refId) m) (map (renderPlainStrict) result))
--	mainLoop []

showRefAsBibtex :: Reference -> ([Reference] -> [[FormattedOutput]]) -> String
showRefAsBibtex r pf = renderPlainStrict $ head $ pf [r]

mainLoop :: [Reference] -> IO ()
mainLoop database = do 
	putStrLn "Please enter a reference"
	ref <- getLine
	if (ref == "q") || (ref == ":q") || (ref == "quit")
		then putStrLn "Quit requested, exiting."
		else if ref == "clear" then do
			putStrLn "Clearing database"
			mainLoop []
		else do 
			lookupResult <- resolveEitherDef ref
			newDb <- updateDatabase database lookupResult
			mainLoop newDb

updateDatabase :: [Reference] -> Either String Reference -> IO [Reference]
updateDatabase database lookupResult = case lookupResult of 
				Left err -> do 
					putStrLn ("Error looking up reference: " ++ err)
					return database
				Right ref -> do
					putStrLn "Adding to database"
					putStrLn "Database contains:"
					strDb <- showReferenceList (ref:database) 
					putStrLn strDb
			 		return (ref:database)

			 		--show (ref:database)

showReferenceList :: [Reference] -> IO (String)
showReferenceList rs = do
	s <- readCSLFile "bibtex.csl"
	let result = processBibliography procOpts s rs
	return $ unlines $ map (renderPlainStrict) result

-- resolveEitherRef s = do
--     fn <- getDataFileName "default.db"
--     let go = withDatabaseFile fn $ ((runEitherT.resolveEither) s)
--     State.evalStateT go (Database Map.empty)



cites :: [Cite]
cites = [emptyCite { citeId = "Caso2007"
                    , citeLabel = "page"
                    , citeLocator = "15"}
         ,emptyCite { citeId = "Rossato2006"
                    , citeLabel = "page"
                    , citeLocator = "10"}
         ]

-- getSimilar word database threshold = filter (\w -> (approxStringMatch word w) >= threshold) database

-- approxStringMatch sa sb = (bigger-ld)/bigger where
-- 	ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 
-- 	bigger = fromIntegral $ max (length sa) (length sb)

-- approxStringMatch' sa sb = ( ((sal-ld)/sal),  ((sbl-ld)/sbl), ld) where
-- 	sal = fromIntegral $ length sa
-- 	sbl = fromIntegral $ length sb
-- 	ld = fromIntegral $ levenshteinDistance defaultEditCosts sa sb 


--type DbEntry = Reference
--type Database = [Reference] 
--	deriving (Show, Eq)
-- type Database = [(String, Int)]