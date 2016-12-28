{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Error.AddUser where

import Control.Monad.Trans.Except

data AddUserError
  = AUE_AlreadyExists
  | AUE_InvalidUsername String
  | AUE_NoPassword
  | AUE_Other String
  deriving Show

type AddUser a = forall m. (Functor m) => ExceptT AddUserError m a
