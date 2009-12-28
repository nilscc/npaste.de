module Paste.View
    ( htmlOpts
    , getLogin
    , htmlBody
    , module App.View
    ) where

import Data.Maybe           (fromMaybe)

import HSP                  (HSP (..), XML (..))
import Happstack.Server
import Happstack.State      (query)

import App.View
import Paste.Types          (LoggedIn (..))
import Paste.View.Menu      (menuHsp)
import Users.State          (SessionID (..), UserOfSessionId (..))
import Users.Auth           (getSessionId)


-- | Get logged in status
getLogin :: ServerPart LoggedIn
getLogin = do
    -- Get session data
    sid     <- getDataFn $ getSessionId
    host    <- askRq >>= return . rqPeer
    user    <- query $ UserOfSessionId (fromMaybe (SessionID 0) sid) host
    return $ case user of
                  Just user -> LoggedInAs user
                  _         -> NotLoggedIn
-- | Default html options
htmlOpts :: [HtmlOptions]
htmlOpts = [ WithCss    (CssFile "/static/style.css")
           , WithTitle  "npaste.de"
           , WithLogo   "npaste.de" "IO String" "a haskell happstack pastebin"
           ]

-- | Default HTML body
htmlBody :: LoggedIn -> [HSP XML] -> HtmlBody
htmlBody login elem = HtmlBody htmlOpts $ [menuHsp login] ++ elem
