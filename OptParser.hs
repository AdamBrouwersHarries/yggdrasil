module Main where

import Options.Applicative

data PaperOptions = PaperOptions {
    filename :: FilePath,
    forceResolution :: Bool
}

paperOptions :: Parser PaperOptions
paperOptions = PaperOptions
    <$> strOption
        ( long "filename" 
            <> short 'f'
            <> metavar "FILENAME"
            <> help "File from which to read paper thoughs." )
    <*> switch
        ( long "force" 
            <> help "Whether to force the resolution of authors and references")

greet :: PaperOptions -> IO ()
greet (PaperOptions filename True) = putStrLn ("Reading file " ++ filename ++ " and forcing")
greet (PaperOptions filename False) = putStrLn ("Reading file " ++ filename ++ " and not forcing")

main :: IO ()
main = execParser opts >>= greet where
    opts = info (helper <*> paperOptions)
        ( fullDesc 
        <> progDesc "yggdrasil, a citation-chasing paper management tool"
        <> header "yggdrasil - the paper management tool")

--data Sample = Sample
--  { hello :: String
--  , quiet :: Bool }

--sample :: Parser Sample
--sample = Sample
--  <$> strOption
--      ( long "hello"
--     <> metavar "TARGET"
--     <> help "Target for the greeting" )
--  <*> switch
--      ( long "quiet"
--     <> help "Whether to be quiet" )

--greet :: Sample -> IO ()
--greet (Sample h False) = putStrLn $ "Hello, " ++ h
--greet _ = return ()

--main :: IO ()
--main = execParser opts >>= greet
--  where
--    opts = info (helper <*> sample)
--      ( fullDesc
--     <> progDesc "Print a greeting for TARGET"
--     <> header "hello - a test for optparse-applicative" )