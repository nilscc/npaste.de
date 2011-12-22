{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NPaste.Types.Database.Session
  ( Session (..)
  ) where

import NPaste.Types.Database.User

data Session = Session
  { sessionId   :: String
  , sessionUser :: Maybe User
  }
  deriving (Eq, Show)
