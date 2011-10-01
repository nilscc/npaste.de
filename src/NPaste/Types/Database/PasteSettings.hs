{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.PasteSettings
  ( PasteSettings (..)
  ) where

import Control.Applicative
import Database.HDBC
import Data.Convertible
import Data.Typeable

data PasteSettings = PasteSettings
  { ps_default_hidden :: Bool
  , ps_default_random :: Bool
  , ps_use_global_ids :: Bool
  }
  deriving (Show, Typeable, Eq)

instance Convertible [SqlValue] PasteSettings where
  safeConvert [h,r,g] =
    PasteSettings <$> safeConvert h
                  <*> safeConvert r
                  <*> safeConvert g
  safeConvert a = convError "" a
