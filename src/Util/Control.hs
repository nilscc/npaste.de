module Util.Control where

import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Happstack.State
import System.Time

import qualified Happstack.Auth as Auth
import qualified Data.Map as M

import Paste.Types.Status
import Users.State


-- | Match on a QUERY element and pass its value to the function
hQuery :: (ServerMonad m, MonadPlus m) => String -> (String -> m a) -> m a
hQuery q f = do
    val <- getDataQueryFn $ look q
    case val of
         Just v | not (null v) -> f v
         _ -> mzero


-- | Run @m a@ only when logged in, returns mzero if not.
withLogin :: (Functor m, MonadIO m, ServerMonad m, MonadPlus m)
          => m a
          -> m a
withLogin m = do
    login <- getLogin
    case login of
         LoggedInAs _ -> m
         NotLoggedIn  -> mzero


-- | Run @m a@ only when *not* logged in, returns mzero if logged in.
withoutLogin :: (Functor m, MonadIO m, MonadPlus m, ServerMonad m)
             => m a
             -> m a
withoutLogin m = do
    login <- getLogin
    case login of
         LoggedInAs _ -> mzero
         NotLoggedIn  -> m


-- | Get logged in status
getLogin :: (Functor m, MonadIO m, ServerMonad m, MonadPlus m)
         => m LoggedIn
getLogin = do

    -- Get session data
    skey' <- fmap Auth.SessionKey `fmap` getDataFn (readCookieValue "session-key")
    case skey' of

         Just skey -> do
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
