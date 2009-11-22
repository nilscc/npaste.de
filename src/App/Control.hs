module App.Control (appHandler) where 

import Happstack.Server
import App.Conf (AppConf (..))
-- import Hack.Handler.Happstack (appToServerPart) -- TODO: CGI

import Control.Monad (mzero)
import Data.List (isPrefixOf, isInfixOf)
import Data.ByteString.Char8 (unpack)
import Text.Regex

-- Applications
import Paste.Control (pasteHandler)


-- | VHost data definition
data VHost = VHost { toMatch :: String
                   , response :: ServerPart Response
                   }

-- | vHosts for appHandler. Regex should work fine... Go from top to bottom and
-- run the first match.
vHosts = [ VHost "localhost" $ pasteHandler -- testing
         , VHost "npaste.de" $ pasteHandler
         , VHost "n-sch.de"  $ fileServe ["index.html"] "n-sch.de"
         ]


-- | Handle incoming events
appHandler :: AppConf -> ServerPart Response
appHandler appConf = if local appConf
                        then pasteHandler
                        else withHost getVHosts

-- | Helper for appHandler, handle virtual hosts
getVHosts host =
    let vh `f` r = maybe r (const (vh:r)) $ matchRegex (mkRegex . toMatch $ vh) host
    in response . last $ case foldr f [] vHosts of
                              [] -> vHosts
                              l  -> l



--------------------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------------------

testing :: ServerPart Response
testing = askRq >>= ok . toResponse . show
