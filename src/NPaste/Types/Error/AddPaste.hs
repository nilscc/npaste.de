{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddPaste where

import Control.Monad.Trans.Except
import Control.Monad.IO.Peel

import NPaste.Types.Database.Paste

data AddPasteError
  = APE_UserRequired
  | APE_NoContent
  | APE_InvalidCustomId String
  | APE_AlreadyExists Paste
  | APE_Other String
  | APE_DescTooLong
  deriving Show

type AddPaste a = (MonadPeelIO m, Functor m) => ExceptT AddPasteError m a
