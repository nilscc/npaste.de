module Util.Control where

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Happstack.Server
import Happstack.State
import System.Time

import qualified Happstack.Auth        as Auth
import qualified Data.Map                       as M

import Paste.Types.Status
import Users.State

npastePolicy :: BodyPolicy
npastePolicy = defaultBodyPolicy "tmp/" 0 1000000 1000000

-- | Remove any trailing white space characters
stripSpaces :: String -> String
stripSpaces = init . unlines . map (foldr strip "") . lines . (++ " ")
  where strip s "" = if isSpace s then "" else [s]
        strip s r  = s : r

-- | Match on a QUERY element and pass its value to the function
hQuery :: (ServerMonad m, MonadIO m, MonadPlus m, HasRqData m) => String -> (String -> m a) -> m a
hQuery q f = do
    val <- getDataFn . queryString $ look q
    case val of
         Right v | not (null v) -> f v
         _ -> mzero


-- | Run @m a@ only when logged in, returns mzero if not.
withLogin :: (Functor m, MonadIO m, ServerMonad m, MonadPlus m, HasRqData m)
          => m a
          -> m a
withLogin m = do
    login <- getLogin
    case login of
         LoggedInAs _ -> m
         NotLoggedIn  -> mzero


-- | Run @m a@ only when *not* logged in, returns mzero if logged in.
withoutLogin :: (Functor m, MonadIO m, MonadPlus m, ServerMonad m, HasRqData m)
             => m a
             -> m a
withoutLogin m = do
    login <- getLogin
    case login of
         LoggedInAs _ -> mzero
         NotLoggedIn  -> m


-- | Get logged in status
getLogin :: (Functor m, MonadIO m, ServerMonad m, MonadPlus m, HasRqData m)
         => m LoggedIn
getLogin = do

    -- Get session data
    skey' <- fmap Auth.SessionKey `fmap` getDataFn (readCookieValue "session-key")
    case skey' of

         Right skey -> do
             host    <- rqPeer `fmap` askRq
             sdata   <- query $ AskSessionData

             now     <- liftIO getClockTime
             let lessThanAMonth ctime = let tdiff = normalizeTimeDiff $ diffClockTimes now ctime
                                            maxT  = normalizeTimeDiff $ noTimeDiff { tdMonth = 1 }
                                        in tdiff <= maxT

             case M.lookup skey sdata of
                  Just (h,ctime) | host == h && lessThanAMonth ctime ->
                      return $ LoggedInAs skey
                  _ -> return NotLoggedIn

         _ -> return NotLoggedIn

requireLogin :: (Functor m, MonadIO m, ServerMonad m, MonadPlus m, HasRqData m)
             => m (Auth.UserId, Auth.Username)
requireLogin = do

    login <- getLogin
    case login of

         LoggedInAs skey -> do

             sdata <- query $ Auth.GetSession skey
             case sdata of

                  Just (Auth.SessionData uid uname) -> return (uid,uname)
                  _ -> mzero

         _ -> mzero
