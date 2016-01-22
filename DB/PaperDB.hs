module DB.PaperDB where
import DB.PaperDef
import YgUtil
import Data.List
import Text.EditDistance
import System.Directory
import Database.HDBC 
import Database.HDBC.Sqlite3


-- getPaper :: Connection -> IO Paper
-- getPaper conn = undefined

--searchByTitle :: Connection -> String -> [Paper]
--searchByTitle conn ptitle = do
--    -- get a list of paper titles from the database
--    titles <- quickQuery conn "SELECT title FROM papers;" []
--    take 5 $ (map fst) . (sortOn snd) . (map levenstein) (flatStrs titles)
--    where
--        flatStrs t = (foldr (++) [] $ map (map fromSql) t) :: [String]
--        levenstein str = (str, 
--            fromIntegral $ levenshteinDistance defaultEditCosts ptitle str)

addPaper :: Paper -> Connection -> IO ()
addPaper p conn = do 
    dPutStrLn $ show p
    (maxid:_):_ <- quickQuery conn "SELECT MAX(id) FROM papers;" []
    dPutStrLn "Done"
    let newid = 1 + (fromSql maxid) :: Integer
    dPutStrLn $ show newid
    res <- run conn "INSERT INTO papers VALUES (?, ?, ?, ?, ?, ?, ?)" 
        [toSql newid, 
         toSql (title p),
         toSql (notes p),
         toSql (context p),
         toSql (contribution p),
         toSql (criticism p),
         toSql (impact p),
         toSql (bibtex p)]
    dPutStrLn ("Inserted " ++ (show res) ++ "rows")

getTitleIDs :: Connection -> IO [(Integer, String)]
getTitleIDs conn = do
    results <- quickQuery' conn "SELECT id, title FORM papers;" []
    return $ map parseRow results where
        parseRow :: [SqlValue] -> (Integer, String)
        parseRow (id:title:[]) = (fromSql id, fromSql title) :: (Integer, String)
        parseRow _ = (-1,"Nothing")

openDB :: FilePath -> IO Connection
openDB dbname = do 
    dbExists <- doesFileExist dbname
    if dbExists then
        connectSqlite3 dbname
    else do
        putStrLn ("Database "++dbname++"doesn't exist, creating...")
        createDB dbname

createDB :: FilePath -> IO Connection
createDB dbname = do
    conn <- connectSqlite3 dbname
    -- create the table for papers
    _ <- run conn ("CREATE TABLE papers (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "title VARCHAR(8192), " ++
        "notes VARCHAR(32768), " ++
        "context VARCHAR(32768), " ++
        "contribution VARCHAR(32768), " ++
        "criticism VARCHAR(32768), " ++
        "impact VARCHAR(32768), " ++
        "bibtex VARCHAR(32768) " ++
        ");") []
    -- create the table for keywords
    _ <- run conn ("CREATE TABLE keywords (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "word VARCHAR(256) " ++
        ");") []
    -- create the table for authors
    _ <- run conn ("CREATE TABLE authors (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "name VARCHAR(256) " ++
        ");") []
    -- create the table for references between papers
    _ <- run conn ("CREATE TABLE refs (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "referer INTEGER NOT NULL, " ++
        "referant INTEGER NOT NULL, " ++
        "FOREIGN KEY(referer) REFERENCES papers(id), " ++
        "FOREIGN KEY(referant) REFERENCES papers(id) " ++
        ");") []
    -- create the table associating authors with papers
    _ <- run conn ("CREATE TABLE apassoc (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "paper INTEGER NOT NULL, " ++
        "author INTEGER NOT NULL, " ++ 
        "FOREIGN KEY(paper) REFERENCES papers(id), " ++
        "FOREIGN KEY(author) REFERENCES authors(id) " ++
        ");") []
    -- create the table associating papers with keywords
    _ <- run conn ("CREATE TABLE kpassoc (" ++
        "id INTEGER NOT NULL PRIMARY KEY, " ++
        "paper INTEGER NOT NULL, " ++
        "keyword INTEGER NOT NULL, " ++
        "FOREIGN KEY(paper) REFERENCES papers(id), " ++ 
        "FOREIGN KEY(keyword) REFERENCES keywords(id) " ++
        ");") []
    commit conn 
    dPutStrLn ("Created db at file: " ++ dbname)
    return conn