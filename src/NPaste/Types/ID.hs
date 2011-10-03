{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Types.ID
  ( IdSetting (..)
  , ID (..)
  ) where

import NPaste.Types.Database.User

--------------------------------------------------------------------------------
-- IDs

data IdSetting
  -- = IdDefault
  = IdRandom
  -- | IdPrivate
  | IdPrivateCustom String

data ID
  = ID String
  | PrivateID User String
  deriving (Eq)

instance Show ID where
  show (ID pid) = "/" ++ pid ++ "/"
  show (PrivateID User{u_name} pid) = "/u/" ++ u_name ++ "/" ++ pid ++ "/"
