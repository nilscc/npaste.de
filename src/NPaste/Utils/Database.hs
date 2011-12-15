{-# LANGUAGE OverloadedStrings #-}

module NPaste.Utils.Database
  ( byteaPack
  , byteaUnpack
  ) where

import Data.Word
import Database.HDBC
import Text.Printf
import Numeric

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8


--------------------------------------------------------------------------------
-- Binary data (postgrsql only)

-- | Convert binary `ByteString` to valid posgresql `SqlValue` (hex escaping)
byteaPack :: B.ByteString -> SqlValue
byteaPack bs = toSql $ "\\x" `B.append` B.concatMap packW8 bs
 where
   packW8 :: Word8 -> B.ByteString
   packW8 = B8.pack . printf "%02x"

-- | Retrieve a binary `ByteString` from a postgresql `SqlValue` (hex escaping)
byteaUnpack :: SqlValue -> B.ByteString
byteaUnpack sql = B.pack . unpackW8 $ fromSql sql
 where
  unpackW8 :: String -> [Word8]
  unpackW8 ('\\':'x':r) = unpackW8 r
  unpackW8 (a:b:c)      = case readHex [a,b] of
                               [(h,"")] -> (h:unpackW8 c)
                               _        -> error $ "invalid sequence: " ++ show [a,b]
  unpackW8 []           = []
  unpackW8 _            = error "uneven number of digits"
