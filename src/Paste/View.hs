module Paste.View
    ( htmlOpts
    , getLogin
    , htmlBody
    , module App.View
    ) where

import Control.Monad.Trans
import HSP                  (HSP, XML (..))
import Happstack.Server
import Happstack.State
import System.Time

import qualified Data.Map as M
import qualified Happstack.Auth as Auth

import App.View
import Paste.Types
import Paste.View.Menu
import Users.State


-- | Get logged in status
getLogin :: ServerPart LoggedIn
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


-- | Default html options
htmlOpts :: [HtmlOptions]
htmlOpts = [ WithCss    (CssFile "/static/style.css")
           , WithTitle  "npaste.de"
           , WithLogo   "npaste.de" "IO String" "a haskell happstack pastebin"
           ]

-- | Default HTML body
htmlBody :: LoggedIn -> [HSP XML] -> HtmlBody
htmlBody login elem = HtmlBody htmlOpts $ [menuHsp login] ++ elem
