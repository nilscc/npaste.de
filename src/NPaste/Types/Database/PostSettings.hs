{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.PostSettings
  ( PostSettings (..)
  ) where

import Control.Applicative
import Database.HDBC
import Data.Convertible
import Data.Typeable

data PostSettings = PostSettings
  { ps_default_hidden :: Bool
  , ps_default_random :: Bool
  , ps_use_global_ids :: Bool
  }
  deriving (Show, Typeable, Eq)

instance Convertible [SqlValue] PostSettings where
  safeConvert [h,r,g] =
    PostSettings <$> safeConvert h
                 <*> safeConvert r
                 <*> safeConvert g
  safeConvert a = convError "" a
