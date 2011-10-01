{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddPaste where

import Control.Monad.Error
import Control.Monad.IO.Peel

import NPaste.Types.Database.User

data AddPasteError
  = APE_UserRequired
  | APE_NoContent
  | APE_InvalidCustomId String
  | APE_AlreadyExists (Maybe User) String
  | APE_Other String
  | APE_DescTooLong
  deriving Show

instance Error AddPasteError where
  strMsg = APE_Other

type AddPaste a = (MonadPeelIO m, Functor m) => ErrorT AddPasteError m a
