module NPaste.Database.Sessions
  ( getSession
  , addSession
  ) where

import Control.Applicative
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
             querySql "SELECT user_id FROM sessions WHERE id = ?, ip = ?, ua = ?"
                      [ toSql sid, toSql ip, toSql ua ]
  mu <- case join mr of
             Just uid -> getUserById uid
             _        -> return Nothing
  return $ Session sid mu <$ mr


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
  return $ Session i mu
 where
  genId g = do
    let i = map (chars !!) . take 15 $ randomRs (0,length chars-1) g
    [[c]] <- querySql "SELECT count(*) FROM sessions WHERE id = ?" [toSql (i :: String)]
    if c == SqlInteger 0 then
       return i
     else
       genId (snd $ next g)
  chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
