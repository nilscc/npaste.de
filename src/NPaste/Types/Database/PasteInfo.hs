{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.PasteInfo
  ( PasteInfo (..)
  ) where

import Control.Applicative
import Database.HDBC
import Data.Convertible
import Data.Time
import Data.Typeable

data PasteInfo = PasteInfo
  { p_id            :: String
  , p_user_id       :: Int
  , p_date          :: UTCTime
  , p_type          :: Maybe String
  , p_description   :: Maybe String
  , p_hidden        :: Bool
  , p_id_is_global  :: Bool
  }
  deriving (Show, Typeable, Eq)

instance Convertible [SqlValue] PasteInfo where
  safeConvert [i,u,d,t,de,h,g] =
    PasteInfo <$> safeConvert i
              <*> safeConvert u
              <*> safeConvert d
              <*> optional (safeConvert t)
              <*> optional (safeConvert de)
              <*> safeConvert h
              <*> safeConvert g
  safeConvert a = convError "" a
