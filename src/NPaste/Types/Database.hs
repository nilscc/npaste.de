{-# LANGUAGE RankNTypes, NamedFieldPuns #-}

module NPaste.Types.Database
  ( Query
  , Update

  , module NPaste.Types.Database.Paste
  , module NPaste.Types.Database.Session
  , module NPaste.Types.Database.User
  ) where

import NPaste.Types.Database.Paste
import NPaste.Types.Database.Session
import NPaste.Types.Database.User

type Query  a = IO a
type Update a = IO a
