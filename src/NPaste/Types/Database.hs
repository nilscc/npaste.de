{-# LANGUAGE RankNTypes #-}

module NPaste.Types.Database where

import Control.Monad.IO.Peel

type Query  a = (Functor m, MonadPeelIO m) => m a
type Update a = (Functor m, MonadPeelIO m) => m a
