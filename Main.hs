{-# LANGUAGE FlexibleContexts #-}
module Main where
import DB.PaperDef
import DB.PaperDB as PDB
import PaperParser as PP
import System.Directory
import System.Console.Repline
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

import Control.Monad.IO.Class   
import Data.List as L
import System.Process
import Control.Monad.State.Strict
import Data.Set as S

data AppState = AppState {
    db :: Maybe PaperDB,
    cwdir :: FilePath, -- need to find neater way to handle file expansion!
    cmpwords :: S.Set String
}

type Repl a = HaskelineT (StateT AppState IO) a


main :: IO ()
main = repl

blankState :: AppState
blankState = AppState {
    db = Nothing,
    cwdir = "/",
    cmpwords = S.empty
}

repl :: IO ()
repl = flip evalStateT blankState
     $ evalRepl ">>> " cmd opts (Prefix (wordCompleter comp) defaultMatcher) ini

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

opts :: [(String, [String] -> Repl ())]
opts = [
    ("help", help), -- :help
    ("say", say), -- :say
    ("spock",spock), -- :spock
    ("add", Main.loadPaper), -- :add
    ("open",Main.openDB), -- :open
    ("puts",puts) -- :puts
    ]



comp :: (Monad m, MonadIO m, MonadState AppState m) => WordCompleter m
comp n = do
    (AppState d cwd cw) <- get -- get the application state
    let commands = pref[":say", ":help", ":puts", ":spock"]
    let words = pref(S.toList cw)
    filesIO <- liftM pref $ liftIO (getDirectoryContents cwd)
    return $ commands ++ words ++ filesIO
    where pref = L.filter (isPrefixOf n)


-- prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":add"    , fileCompleter),
    (":open"    , fileCompleter)
    --,
  --, (":holiday" , listCompleter ["christmas", "thanksgiving", "festivus"])
    ]


-- Add a string to the database of comparison words
puts :: [String] -> Repl ()
puts args = do
    if head args /= "cheese" then
        modify $ \s -> s {cmpwords = S.union (cmpwords s) (S.fromList args)}
    else 
        liftIO $ print "cannot add cheese to the database!"

-- Commands 
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
  _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
  return ()

loadPaper :: [String] -> Repl ()
loadPaper [] = liftIO $ print "Please specify a file to load"
loadPaper (fname:[]) = liftIO $ do
    exists <- doesFileExist fname
    case exists of 
        True -> print "Loaded paper!"
        False -> print "Failed to load paper!"
loadPaper _ = liftIO $ print "Please specify a single paper to load"

openDB ::  [String] -> Repl ()
openDB [] = liftIO $ print "Please specify a file to load"
openDB (dbname:[]) = liftIO $ do
    exists <- doesFileExist dbname
    case exists of 
        True -> do
            print "Opened database!"
            dbConn <- PDB.openDB dbname
            case dbConn of 
                Just conn -> do
                    liftIO $ print "Got a conncetion!"
                    --modify $ \s -> s {db = Just (PaperDB conn)}
                Nothing -> print "Failed to open database!"
        False -> print "Failed to open database!"
openDB _ = liftIO $ print "Please specify a single database to open"

spock :: [String] -> Repl ()
spock args = liftIO $ print "Live long and prosper!"



--main' :: IO ()
--main' = do
--    paper <- getPaper
--    putStrLn $ show paper
--    --PDB.addPaper paper
    
--getPaper :: IO (Either String Paper)
--getPaper = do
--    putStrLn "Please enter the filename of a paper to load: "
--    filename <- getLine
--    exists <- doesFileExist filename
--    case exists of
--        True -> loadPaper filename 
--        False -> return $ Left "File not found"