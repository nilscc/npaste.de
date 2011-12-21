{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.Session
  ( Session (..)
  ) where

import Control.Applicative
import Data.Convertible
import Data.Typeable
import Database.HDBC

newtype Session = Session { unSession :: String }
  deriving (Eq, Show, Typeable)

instance Convertible [SqlValue] Session where
  safeConvert [i] = Session <$> safeConvert i
  safeConvert a   = convError "" a
