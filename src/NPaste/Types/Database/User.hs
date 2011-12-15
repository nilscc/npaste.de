{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.User
  ( User (..)
  ) where

import Control.Applicative
import Database.HDBC
import Data.Convertible
import Data.Typeable

data User = User
  { u_id             :: Int
  , u_name           :: String
  , u_email          :: Maybe String
  , u_default_hidden :: Bool
  }
  deriving (Show, Typeable, Eq)

instance Convertible [SqlValue] User where
  safeConvert [i,n,e,h] =
    User <$> safeConvert i
         <*> safeConvert n
         <*> optional (safeConvert e)
         <*> safeConvert h
  safeConvert a = convError "" a
