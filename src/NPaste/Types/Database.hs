{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NPaste.Types.Database
  ( Query(Query)
  , Update(Update)

  , runQuery
  , runUpdate
  , liftQuery

  , module NPaste.Types.Database.Paste
  , module NPaste.Types.Database.Session
  , module NPaste.Types.Database.User
  ) where

import Control.Monad.Trans

import NPaste.Types.Database.Paste
import NPaste.Types.Database.Session
import NPaste.Types.Database.User

newtype Query  a = Query  { unQuery  :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
newtype Update a = Update { unUpdate :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runQuery :: MonadIO m => Query a -> m a
runQuery = liftIO . unQuery

runUpdate :: MonadIO m => Update a -> m a
runUpdate = liftIO . unUpdate

liftQuery :: Query a -> Update a
liftQuery (Query q) = Update q
