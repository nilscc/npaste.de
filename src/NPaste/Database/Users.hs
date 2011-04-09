module NPaste.Database.Users where

import Control.Monad.Trans
import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils

getUserById :: MonadIO m => Int -> m (Maybe User)
getUserById uid = do
  res <- querySql "SELECT u_id, u_name, u_email FROM users \
                  \WHERE u_id = ? AND u_id >= 0"
                  [toSql uid]
  return $ convertListToMaybe res

getAllUsers :: (Functor m, MonadIO m) => m [User]
getAllUsers = do
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT u_id, u_name, u_email FROM users \
                 \WHERE u_id >= 0" [])
