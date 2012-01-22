{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.Session
  ( Session (..)
  ) where

import Data.Time

import NPaste.Types.Database.User

data Session = Session
  { sessionId      :: String
  , sessionExpires :: UTCTime
  , sessionUser    :: Maybe User
  }
  deriving (Eq, Show)
