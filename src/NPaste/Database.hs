module NPaste.Database
  ( -- * Users
    module NPaste.Database.Users
  , numberOfUsers
    -- * Pastes
  , module NPaste.Database.Pastes
  , numberOfPastes
  ) where

import NPaste.Database.Users
import NPaste.Database.Pastes

import Control.Monad.Trans
import NPaste.Database.Connection

numberOfPastes :: MonadIO m => m Integer
numberOfPastes = do
  [[SqlInteger i]] <- querySql "SELECT count(*) FROM pastes" []
  return i

numberOfUsers :: MonadIO m => m Integer
numberOfUsers = do
  [[SqlInteger i]] <- querySql "SELECT count(*) FROM users WHERE NOT id = '-1'" []
  return i
