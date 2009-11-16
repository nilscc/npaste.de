module App.Control (appHandler) where 

import Happstack.Server
import App.Conf (AppConf (..))

import Control.Monad (mzero)
import Data.List (isPrefixOf, isInfixOf)
import Data.ByteString.Char8 (unpack)
import Text.Regex

-- Applications
import Paste.Control (pasteHandler)


-- | VHost data definition
data VHost = VHost { toMatch :: String
                   , response :: ServerPartT IO Response
                   }

-- | vHosts for appHandler. Regex should work fine...
vHosts = [ VHost "n-sch.de"  $ fileServe ["index.html"] "n-sch.de"
         , VHost "npaste.de" $ pasteHandler
         , VHost "localhost" $ testing
         ]


-- | Handle incoming events
appHandler :: AppConf -> ServerPartT IO Response
appHandler appConf = if local appConf
                        then pasteHandler
                        else withHost getVHosts

-- | Helper for appHandler, handle virtual hosts
getVHosts host =
    let vh `f` r = maybe r (const (vh:r)) $ matchRegex (mkRegex . toMatch $ vh) host
    in response . head $ case foldr f [] vHosts of
                              [] -> vHosts
                              l  -> l





--------------------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------------------

testing = getHeaderM "host" >>= ok . toResponse . show
