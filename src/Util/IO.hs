{-# LANGUAGE ScopedTypeVariables #-}
module Util.IO where

import System.IO
import Codec.Binary.UTF8.Light
import qualified Control.Exception as E

-- | Strict version of "readFile". Closes the handle after reading the whole
-- file
readFile' :: FilePath -> IO String
readFile' name = withFile name ReadMode $ \h -> do
    c <- E.catch (hGetUTF8Contents h)
                 (\(_ :: E.SomeException) -> do return "Invalid paste.")
    length c `seq` return c
