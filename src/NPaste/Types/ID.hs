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
  deriving Show
