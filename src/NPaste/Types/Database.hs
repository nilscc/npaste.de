{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Database
  ( Query
  , Update

  , module NPaste.Types.Database.PasteInfo
  , module NPaste.Types.Database.PasteSettings
  , module NPaste.Types.Database.User
  ) where

import Control.Monad.IO.Peel

import NPaste.Types.Database.PasteInfo
import NPaste.Types.Database.PasteSettings
import NPaste.Types.Database.User

type Query  a = (Functor m, MonadPeelIO m) => m a
type Update a = (Functor m, MonadPeelIO m) => m a
