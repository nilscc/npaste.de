{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddUser where

import Control.Monad.Trans.Except
import Control.Monad.IO.Peel

data AddUserError
  = AUE_AlreadyExists
  | AUE_InvalidUsername String
  | AUE_NoPassword
  | AUE_Other String
  deriving Show

type AddUser a = (MonadPeelIO m, Functor m) => ExceptT AddUserError m a
