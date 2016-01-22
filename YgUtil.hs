module YgUtil where

debugMode :: Bool
debugMode = False

dPutStrLn :: String -> IO ()
dPutStrLn message = if debugMode then putStrLn message else putStr ""