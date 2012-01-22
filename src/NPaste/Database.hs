module NPaste.Database
  ( -- * Users
    module NPaste.Database.Users
  , getNumberOfUsers
    -- * Sessions
  , module NPaste.Database.Sessions
    -- * Pastes
  , module NPaste.Database.Pastes
  , getNumberOfPastes
  ) where

import NPaste.Database.Users
import NPaste.Database.Pastes
import NPaste.Database.Sessions

import Control.Monad.Trans
import NPaste.Database.Connection

getNumberOfPastes :: MonadIO m => m Integer
getNumberOfPastes = do
  [[SqlInteger i]] <- querySql "SELECT count(*) FROM pastes" []
  return i

getNumberOfUsers :: MonadIO m => m Integer
getNumberOfUsers = do
  [[SqlInteger i]] <- querySql "SELECT count(*) FROM active_users WHERE NOT id = '-1'" []
  return i
