{-# LANGUAGE RankNTypes, NamedFieldPuns #-}

module NPaste.Types.Database
  ( Query
  , Update

  , Paste (..)
  , module NPaste.Types.Database.PasteInfo
  , module NPaste.Types.Database.PasteSettings
  , module NPaste.Types.Database.User
  ) where

import Control.Monad.IO.Peel
import Data.ByteString (ByteString)
import Data.Time

import NPaste.Types.Database.PasteInfo
import NPaste.Types.Database.PasteSettings
import NPaste.Types.Database.User
import NPaste.Types.ID
import NPaste.Types.Description

type Query  a = (Functor m, MonadPeelIO m) => m a
type Update a = (Functor m, MonadPeelIO m) => m a

data Paste = Paste
  { pasteId         :: ID
  , pasteUser       :: Maybe User
  , pasteDate       :: UTCTime
  , pasteType       :: Maybe String
  , pasteDesc       :: Maybe Description
  , pasteHidden     :: Bool
  , pasteContent    :: ByteString
  }
