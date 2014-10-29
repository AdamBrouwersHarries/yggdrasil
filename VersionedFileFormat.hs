module VersionedFileFormat where
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec

--main = do 
--	putStrLn $ show (FileVersion [1,2,3,4,5])
--	putStrLn $ show (sort [FileVersion [1,1,0], FileVersion [1,2,0], FileVersion [1,0,1]])
--	contents <- readVFFile "datastore.ds" (M.fromList [((FileVersion [0,1,1]), (lines))])
--	case contents of
--		Left e -> putStrLn e
--		Right d -> putStrLn $ show $ d


-- Define a file version type, to allow us to adequately index our parsing functions
-- We want the type to be orderable, to allow us to use it in a Map
-- We want the type to be "showable" to allow us to use it when we're printing to files
data FileVersion = FileVersion [Int] 
	deriving (Eq, Read)
instance Ord FileVersion where
	FileVersion xs `compare` FileVersion ys = xs `compare` ys
instance Show FileVersion where
	show (FileVersion xs) = "[version "++ (tail $ foldl (\a b -> a++"."++b) "" $ map show xs) ++"]"

-- Parse a file version data stucture from a string
fileversion :: Parser FileVersion
fileversion = do 
	string "[version "
	x <- (read <$> many1 digit)
	xs <- manyTill (char '.'*> (read <$> many1 digit)) (char ']')
	return (FileVersion (x:xs))

-- readVFFile - Read a versioned, formatted file
-- We need to provide it with a file name, and a map of file versions to handling functions
-- returns an "either" type, to allow us to keep track of any errors in the process
readVFFile :: String -> (M.Map FileVersion (String -> a)) -> IO (Either String a)
readVFFile fName fParsers = do
	fContents <- readFile fName
	let versionString = head $ lines fContents
	return $ do 
		version <- case parse fileversion "Failed to parse version string" versionString of
			Left e -> Left $ show e
			Right r -> Right r
		parsedFile <- case M.lookup version fParsers of
			Nothing -> Left $ "Failed to find matching parser for this version. Got: " ++ (show version)
			Just f -> Right $ (f $ unlines $ tail $ lines fContents)
		return parsedFile
