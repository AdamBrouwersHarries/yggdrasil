module Main where
import DB.PaperDef
import DB.PaperDB as PDB
import PaperParser as PP
--import OptParser as OP

main :: IO ()
main = do
    paper <- getPaper
    putStrLn $ show paper
    --PDB.addPaper paper
    
getPaper :: IO Paper
getPaper = do
    putStrLn "Paper title: "
    title <- getLine 
    putStrLn "General notes on the paper: "
    notes <- getLine
    putStrLn "Context of the paper: "
    context <- getLine
    putStrLn "Key contribution of the paper: "
    contribution <- getLine
    putStrLn "Critical reflection: "
    criticism <- getLine
    putStrLn "Impact: "
    impact <- getLine
    putStrLn "Bibtex: "
    bibtex <- getLine
    putStrLn "Important references: "
    refs <- getReferences []
    return (
        Paper {
            title = title,
            notes = notes,
            context = context,
            contribution = contribution,
            criticism = criticism,
            impact = impact,
            bibtex = bibtex,
            keywords = [],
            authors = [],
            references = refs 
        })

getReferences :: [String] -> IO [String]
getReferences refs = do
    putStrLn $ "Reference " ++ (show $ length refs)
    ref <- getLine
    case ref of 
        "" -> return $ reverse refs
        _  -> getReferences (ref:refs)
