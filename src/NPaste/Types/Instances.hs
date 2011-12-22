{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, TupleSections #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.Instances where

import Control.Concurrent.MState
import Control.Monad.Error
import Control.Monad.IO.Peel
import Data.Convertible
import Database.HDBC
import Happstack.Server
import Happstack.Server.Internal.MonadPeelIO ()

import NPaste.Parser.Description

instance Convertible [SqlValue] Description where
  safeConvert s = fmap (parseDesc . concat) $ mapM safeConvert s

instance Convertible SqlValue Description where
  safeConvert s = fmap (parseDesc)          $      safeConvert s

--------------------------------------------------------------------------------
-- * Happstack instances

-- ** Server Monad instances

instance (MonadIO m, ServerMonad m) => ServerMonad (MState t m) where
  askRq       = lift askRq
  localRq f m = mapMState_ (localRq f) m

-- ** FilterMonad instances

instance (MonadIO m, FilterMonad a m) => FilterMonad a (MState t m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter m   = mapMState_ getFilter m

instance (Error err, FilterMonad a m) => FilterMonad a (ErrorT err m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter m   = do (errT, aa) <- lift $ getFilter (runErrorT m)
                     either throwError (return . (,aa)) errT

-- ** WebMonad instances

instance (Error err, WebMonad a m) => WebMonad a (ErrorT err m) where
  finishWith = lift . finishWith

instance WebMonad a m => WebMonad a (MState t m) where
  finishWith = lift . finishWith

-- ** HasRqData instances

instance (MonadPlus m, MonadPeelIO m, HasRqData m) => HasRqData (MState t m) where
  askRqEnv       = lift askRqEnv
  localRqEnv f m = mapMState_ (localRqEnv f) m
  rqDataError  _ = mzero -- lift $ rqDataError e

instance (Error err, MonadIO m, HasRqData m) => HasRqData (ErrorT err m) where
  askRqEnv       = lift askRqEnv
  localRqEnv f m = mapErrorT (localRqEnv f) m
  rqDataError  _ = mzero -- lift $ rqDataError e
