module App.Control (appHandler) where 

import Happstack.Server
import App.Conf (AppConf (..))
-- import Hack.Handler.Happstack (appToServerPart) -- TODO: CGI

{-
import Control.Monad (mzero)
import Data.List (isPrefixOf, isInfixOf)
import Data.ByteString.Char8 (unpack)
import Text.Regex
-}

-- Applications
import Paste.Control (pasteHandler)


-- | Handle incoming events
appHandler :: AppConf -> ServerPart Response
appHandler _ = pasteHandler -- withHost getVHosts


{-

-- | VHost data definition
data VHost = VHost { toMatch :: String
                   , response :: ServerPart Response
                   }

-- | vHosts for appHandler. Regex should work fine... Go from top to bottom and
-- run the first match.
vHosts = [ VHost ""             $ pasteHandler -- default
         , VHost "n-sch.de"     $ fileServe ["index.html"] "n-sch.de"
         , VHost "wsw.n-sch.de" $ fileServe [] "/home/nils/.warsow-0.5"
         ]

-- | Helper for appHandler, handle virtual hosts
getVHosts host =
    let vh `f` r = maybe r (const (vh:r)) $ matchRegex (mkRegex . toMatch $ vh) host
    in response . last $ case foldr f [] vHosts of
                              [] -> vHosts
                              l  -> l

-}
