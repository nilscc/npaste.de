module Util.IO where

import System.IO

-- | Strict version of "readFile". Closes the handle after reading the whole
-- file
readFile' :: FilePath -> IO String
readFile' name = withFile name ReadMode $ \h -> do
    c <- hGetContents h
    length c `seq` return c
