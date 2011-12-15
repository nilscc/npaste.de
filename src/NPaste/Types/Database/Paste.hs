{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.Database.Paste
  ( Paste (..)
  , Id
  ) where

import Control.Applicative
import Data.ByteString
import Database.HDBC
import Data.Convertible
import Data.Time
import Data.Typeable

import NPaste.Utils.Database (byteaUnpack)

type Id = String

data Paste = Paste
  { p_id            :: Id
  , p_user_id       :: Int
  , p_date          :: UTCTime
  , p_type          :: Maybe String
  , p_description   :: Maybe String
  , p_content       :: ByteString
  , p_hidden        :: Bool
  }
  deriving (Show, Typeable, Eq)

instance Convertible [SqlValue] Paste where
  safeConvert [i,u,d,t,de,cont,h] =
    Paste <$> safeConvert i
          <*> safeConvert u
          <*> safeConvert d
          <*> optional (safeConvert t)
          <*> optional (safeConvert de)
          <*> Right (byteaUnpack cont)
          <*> safeConvert h
  safeConvert a = convError "" a

instance Convertible [SqlValue] Id where
  safeConvert [s] = safeConvert s
  safeConvert a   = convError "" a
