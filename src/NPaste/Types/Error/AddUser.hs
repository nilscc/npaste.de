{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddUser where

import Control.Monad.Error
import Control.Monad.IO.Peel

data AddUserError
  = AUE_AlreadyExists
  | AUE_InvalidUsername String
  | AUE_NoPassword
  | AUE_Other String
  deriving Show

instance Error AddUserError where
  strMsg = AUE_Other

type AddUser a = (MonadPeelIO m, Functor m) => ErrorT AddUserError m a
