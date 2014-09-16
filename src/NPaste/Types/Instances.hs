{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.Instances where

import Control.Applicative
import Control.Concurrent.MState
import Control.Monad.Except
import Control.Monad.IO.Peel
import qualified Data.ByteString as B
import Data.Convertible
import Data.Time
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Database.HDBC
import Happstack.Server
import Happstack.Server.MonadPeel ()
import Text.Blaze.Html

import NPaste.Parser.Description


--------------------------------------------------------------------------------
-- * Convertible instances

instance Convertible [SqlValue] Description where
  safeConvert s = fmap (parseDesc . concat) $ mapM safeConvert s

instance Convertible SqlValue Description where
  safeConvert s = fmap (parseDesc)          $      safeConvert s

instance Convertible [SqlValue] String where
  safeConvert [s] = safeConvert s
  safeConvert a   = convError "" a

instance Convertible [SqlValue] Int where
  safeConvert [i] = safeConvert i
  safeConvert a   = convError "" a

instance Convertible [SqlValue] (UTCTime, Maybe Int) where
  safeConvert [t,i] = (,) <$> safeConvert t
                          <*> safeConvert i
  safeConvert a     = convError "" a


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

-- ** WebMonad instances

instance WebMonad a m => WebMonad a (MState t m) where
  finishWith = lift . finishWith

-- ** HasRqData instances

instance (MonadPlus m, MonadPeelIO m, HasRqData m) => HasRqData (MState t m) where
  askRqEnv       = lift askRqEnv
  localRqEnv f m = mapMState_ (localRqEnv f) m
  rqDataError  _ = mzero -- lift $ rqDataError e


--------------------------------------------------------------------------------
-- * HTML instances

instance ToMarkup B.ByteString where
  toMarkup = toHtml . decodeUtf8With ignore

instance ToValue B.ByteString where
  toValue = toValue . decodeUtf8With ignore
