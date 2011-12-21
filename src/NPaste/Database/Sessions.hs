module NPaste.Database.Sessions
  ( getSession
  , addSession
  ) where

import Data.Time
import System.Random

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils

getSession :: Maybe User
           -> String        -- ^ IP
           -> String        -- ^ User agent
           -> Query (Maybe Session)
getSession mu ip ua = do
  removeOldSessions
  fmap convertListToMaybe $
       querySql ( "SELECT id FROM sessions WHERE ip = ?, user_agent = ?" ++
                  maybe "user_id IS NULL" (const "user_id = ?") mu )
                ( [ toSql ip, toSql ua ] ++
                  maybe [] (return . toSql . userId) mu )

removeOldSessions :: Update ()
removeOldSessions = do
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
  return $ Session i
 where
  genId g = do
    let i = map (chars !!) . take 15 $ randomRs (0,length chars-1) g
    [[c]] <- querySql "SELECT count(*) FROM sessions WHERE id = ?" [toSql (i :: String)]
    if c == SqlInteger 0 then
       return i
     else
       genId (snd $ next g)
  chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
