module DB.PaperDef where

data Paper = Paper {
    title :: String, -- the title of the paper
    notes :: String, --any notes about the paper, in LaTeX format
    context :: String, -- some context for the work
    contribution :: String, -- what's the key contribution of the paper
    criticism :: String, -- critical thoughts about the paper
    impact :: String, -- how well recieved has the paper been
    bibtex :: String, -- a bibtex entry for the paper
    authors :: [String], -- the authors of the paper
    keywords :: [String], -- keywords for the paper (relevant to me)
    references :: [String] -- some important references to chase from this paper
} deriving (Show, Eq)

blankPaper :: Paper
blankPaper = Paper {
        title = "",
        notes = "",
        context = "",
        contribution = "",
        criticism = "",
        impact = "",
        bibtex = "",
        authors = [],
        keywords = [],
        references = []
    }
