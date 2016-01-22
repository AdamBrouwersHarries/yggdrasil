module PaperParser where
import Data.Text (pack, unpack)
import Data.Maybe
import Control.Monad
-- Small markdown library to parse the file
import CMark
-- the definition of an academic paper reading
import DB.PaperDef

--------------------------------------------------------------------------------
------------------- High level interface to load/parse papers ------------------
--------------------------------------------------------------------------------

--- Load a paper from a given filepath
loadPaper :: FilePath -> IO (Either String Paper)
loadPaper path = readTree path >>= (return . docToPaper)

-- read a filepath into a document structure
readTree :: FilePath -> IO Node
readTree path = liftM ((commonmarkToNode []) . pack) (readFile path) 

-- try to convert a document structure into a paper structure 
docToPaper :: Node -> Either String Paper
docToPaper n = do
    -- first, get a list of doc sections from the node
    sections <- parseSections n
    -- extract each of the various sections we need
    title <- (Right . sectionAsLaTeX) =<< findSection "Title" sections
    notes <- (Right . sectionAsLaTeX) =<< findSection "Notes" sections
    context <- (Right . sectionAsLaTeX) =<< findSection "Context" sections
    contribution <- (Right . sectionAsLaTeX) =<< findSection "Contribution" sections
    criticism <- (Right . sectionAsLaTeX) =<< findSection "Criticism" sections
    impact <- (Right . sectionAsLaTeX) =<< findSection "Impact" sections
    bibtex <- sectionAsCodeBlock =<< findSection "Bibtex" sections
    authors <- sectionAsList =<< findSection "Authors" sections
    keywords <- sectionAsList =<< findSection "Keywords" sections
    references <- sectionAsList =<< findSection "References" sections
    -- build, and return, a paper from the sections
    Right (Paper title notes context contribution criticism impact bibtex authors keywords references)

--------------------------------------------------------------------------------
--- Utilities to extract a list of sections from a paper, and search the list --
--------------------------------------------------------------------------------

-- a definition of a section of the paper
data DocSection = Section String [Node]

-- search through a list of DocSections for a given section
findSection :: String -> [DocSection] -> Either String DocSection
findSection title [] = Left ("Failed to find "++title++" section.")
findSection title (d:ds) = if matchSection title d then Right d
    else findSection title ds where
        matchSection :: String -> DocSection -> Bool
        matchSection title (Section st _) = title == st

-- try to group a document into a list of sections
parseSections :: Node -> Either String [DocSection]
parseSections n@(Node l DOCUMENT nc) = do
    -- group a list of nodes by headers
    let groupByHeader :: [Node] -> [[Node]]
        groupByHeader [] = []
        groupByHeader (n:ns) = let sp = break isHeader ns in
            (n : fst sp) : (groupByHeader (snd sp))
            where 
                isHeader :: Node -> Bool
                -- only match on top level header!
                isHeader (Node _ (HEADING 1) _) = True 
                -- fail on anything else
                isHeader (Node _ _ _)          = False
    -- parse a section from a list of nodes
    let makeSection :: [Node] -> Either String DocSection
        makeSection (header:nodes) = do
            -- get the header data from the header node
            (text, level) <- getHeaderData header
            -- ensure the header is a top level header
            if level == 1 then
                Right (Section text nodes)
            else
                Left ("Cannot make a section from a non top level header: " ++ 
                    (showNode n))
    -- group the list of subnodes of the document into (header:nodes) lists
    -- then try to build sections out of each list
    sections <- mapM makeSection $ groupByHeader nc
    -- return the parsed sections
    return sections
parseSections n@(Node _ _ _) = Left "Parse failed: top level node must be a document!"

-------------------------------------------------------------------------------- 
--- Utilities for parsing individual sections of the paper into a useful form --
--------------------------------------------------------------------------------

-- turn a section into a single LaTeX string, by building a new doc
sectionAsLaTeX :: DocSection -> String
sectionAsLaTeX ds = unpack $ nodeToLaTeX options Nothing $ getNewDoc ds where
    options = [optNormalize, optHardBreaks, optSmart]
    getNewDoc (Section _ nodes) = Node Nothing DOCUMENT nodes

-- parse a section into a list of strings, possibly as LaTeX (?)
sectionAsList :: DocSection -> Either String [String]
sectionAsList (Section _ (n@(Node _ (LIST _) sn):nodes)) = getListEntries n
sectionAsList (Section _ sn) = 
    Left ("Failed to parse section as list: " ++ (foldl (++) [] $ map showNode sn))

-- given a section, assume the first node is a code block, and extract it
sectionAsCodeBlock :: DocSection -> Either String String
sectionAsCodeBlock (Section _ ((Node _ (CODE_BLOCK _ t) _):_)) = Right (unpack t)
sectionAsCodeBlock (Section _ sn) = 
    Left ("Cannot parse a code block from: " ++ (foldl (++) [] $ map showNode sn))

-------------------------------------------------------------------------------- 
------ Utilities for parsing parsing data from common structures of node -------
--------------------------------------------------------------------------------

-- try to get the level and text from a header
getHeaderData :: Node -> Either String (String, Int)
getHeaderData n@(Node l (HEADING lv) (tn:sn)) = 
    case tn of
        Node _ (TEXT t) _ -> Right (unpack t, lv)
        _ -> Left ("No text in header node: " ++ (showNode n))
getHeaderData n@(Node _ _ _) = 
    Left ("Cannot parse a non-header node: " ++ (showNode n))

-- get the text from a paragraph node
-- TODO: are there any corner cases that I need to try and cover?
getParagraphText :: Node -> Either String String
getParagraphText n@(Node l PARAGRAPH (tn:sn)) =
    case tn of 
        Node _ (TEXT t) _ -> Right (unpack t) 
        _ -> Left ("No text in paragraph node: " ++ (showNode n)) 
getParagraphText n@(Node _ _ _) = 
    Left ("Cannot parse a non-paragraph node: " ++ (showNode n))

-- get the entries from a list node, as a list of strings
getListEntries :: Node -> Either String [String]
getListEntries n@(Node l (LIST _) sn) = mapM getItemText sn where
    -- get the text from an individual item in a list
    getItemText :: Node -> Either String String
    getItemText n@(Node l ITEM (pn:sn)) = getParagraphText pn
    getItemText n@(Node _ _ _) = 
        Left ("Cannot parse a non-item node: " ++ (showNode n))
getListEntries n@(Node _ _ _) = 
    Left ("Cannot parse a non-list node: " ++ (showNode n))

-------------------------------------------------------------------------------- 
-------------- Printing utilities for better formatting of nodes ---------------
--------------------------------------------------------------------------------

-- nice interface for printing a node
showNode :: Node -> String
showNode = show' 0

-- a better definition for showing a doc
show' :: Int -> Node -> String
show' il (Node _ nt nc) = indent ++ nodeStr ++ " {\n" ++ childStr ++ indent ++ "}\n" 
    where
    childStr :: String
    childStr = if nc == [] then "" else foldl (++) "" $ map (show' (il+1)) nc 
    indent :: String
    indent = foldl (++) "" $ take il $ repeat "  "
    nodeStr :: String 
    nodeStr = case nt of 
        DOCUMENT -> "DOCUMENT"
        THEMATIC_BREAK -> "THEMATIC_BREAK"
        PARAGRAPH -> "PARAGRAPH"
        BLOCK_QUOTE -> "BLOCK_QUOTE"
        HTML_BLOCK t -> "HTML_BLOCK " ++ (unpack t)
        CUSTOM_BLOCK _ _ -> "CUSTOM_BLOCK"
        CODE_BLOCK _ t -> "CODE_BLOCK " ++ (unpack t)
        HEADING l -> "HEADING " ++ (show l)
        LIST _ -> "LIST: "
        ITEM -> "ITEM"
        TEXT t -> "TEXT " ++ (unpack t)
        SOFTBREAK -> "SOFTBREAK"
        LINEBREAK -> "LINEBREAK"
        HTML_INLINE t -> "HTML_INLINE " ++ (unpack t)
        CUSTOM_INLINE _ _ -> "CUSTOM_INLINE"
        CODE t -> "CODE " ++ (unpack t)
        EMPH -> "EMPH"
        STRONG -> "STRONG"
        LINK u t -> "LINK " ++ (unpack u) ++ (unpack t)
        IMAGE u t -> "IMAGE " ++ (unpack u) ++ (unpack t)
