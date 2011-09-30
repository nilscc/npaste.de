{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddPost where

import Control.Monad.Error
import Control.Monad.IO.Peel

import NPaste.Types.Database.User

data AddPostError
  = APE_UserRequired
  | APE_NoContent
  | APE_InvalidCustomId String
  | APE_AlreadyExists (Maybe User) String
  | APE_Other String
  | APE_DescTooLong
  deriving Show

instance Error AddPostError where
  strMsg = APE_Other

type AddPost a = (MonadPeelIO m, Functor m) => ErrorT AddPostError m a
