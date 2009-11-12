module App.Control (appHandler) where 

import Happstack.Server
import App.Conf (AppConf (..))

import Control.Monad (mzero)
import Data.List (isPrefixOf, isInfixOf)
import Data.ByteString.Char8 (unpack)
import Text.Regex

-- Applications
import Paste.Control (pasteHandler)


-- | vHosts for appHandler. Regex should work fine...
vHosts = [ VHost "n-sch.de"  $ fileServe ["index.html"] "n-sch.de"
         , VHost "npaste.de" $ pasteHandler
         , VHost "localhost" $ pasteHandler
         ]


-- | Handle incoming events
appHandler :: AppConf -> ServerPartT IO Response
appHandler appConf = if local appConf
                        then pasteHandler
                        else askRq >>= getVHosts . getHeader "host"

-- | Helper for appHandler, handle virtual hosts
getVhosts Nothing   = response . head $ vHosts
getVHosts (Just bs) =
    let host   = unpack bs
        f vh r = maybe r (const (vh:r)) $ matchRegex (mkRegex . toMatch $ vh) host
    in response . head $ case foldr f [] vHosts of
                              [] -> vHosts
                              l  -> l

-- | VHost data definition
data VHost = VHost { toMatch :: String
                   , response :: ServerPartT IO Response
                   }
