module VersionedFileFormat where


-- readVFFile - Read a versioned, formatted file
-- Filename -> (version, versionParser) -> Data
readVFFile :: String -> (String, (String -> a)) -> a