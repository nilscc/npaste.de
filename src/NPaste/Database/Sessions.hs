{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module NPaste.Database.Sessions
  ( getSession
  , addSession
  , rmSession
  ) where

import Data.Time
import System.Random

import NPaste.Database.Connection
import NPaste.Database.Users
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- * Queries

getSession :: String      -- ^ session ID
           -> String      -- ^ IP
           -> String      -- ^ user agent
           -> Query (Maybe Session)
getSession sid ip ua = do
  removeOldSessions
  mr <- fmap convertListToMaybe $
             querySql "SELECT expires, user_id FROM sessions \
                      \ WHERE id = ? AND ip = ? AND user_agent = ?"
                      [ toSql sid, toSql ip, toSql ua ]
  case mr of
       Just (expires, Just uid) -> do
         mu <- getUserById uid
         return . Just $ Session sid expires mu
       Just (expires, Nothing) ->
         return . Just $ Session sid expires Nothing
       _ ->
         return Nothing


--------------------------------------------------------------------------------
-- * Updates

removeOldSessions :: Update ()
removeOldSessions =
  updateSql_ "DELETE FROM sessions WHERE expires < now()" []

addSession :: Maybe User
           -> String          -- ^ IP
           -> String          -- ^ User agent
           -> Update Session
addSession mu ip ua = do
  now <- liftIO getCurrentTime
  let expires = addUTCTime (60 * 60 * 24 * 30) now
  g   <- liftIO newStdGen
  i   <- genId g
  updateSql_ "INSERT INTO sessions(id, ip, user_agent, expires, user_id)\
             \     VALUES         (? , ? , ?         , ?      , ?      )"
             [ toSql i, toSql ip, toSql ua, toSql expires, toSql (fmap userId mu) ]
  return $ Session i expires mu
 where
  genId g = do
    let i = map (chars !!) . take 15 $ randomRs (0,length chars-1) g
    [[c]] <- querySql "SELECT count(*) FROM sessions WHERE id = ?" [toSql (i :: String)]
    if c == SqlInteger 0 then
       return i
     else
       genId (snd $ next g)
  chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

rmSession :: Session
          -> Update ()
rmSession Session{ sessionId } =
  updateSql_ "DELETE FROM sessions WHERE id = ?" [toSql sessionId]
