module App.Control (appHandler) where 

import Happstack.Server
import App.Conf (AppConf (..))

import Control.Monad (mzero)
import Data.List (isPrefixOf, isInfixOf)
import Data.ByteString.Char8 (unpack)

-- Applications
import Paste.Control (pasteHandler)

appHandler :: AppConf -> ServerPartT IO Response
appHandler appConf = if not (local appConf)
                        then askRq >>= withVHost . maybe "" unpack . getHeader "host"
                        else pasteHandler

withVHost host
    | "npaste.de" `isInfixOf` host    = pasteHandler
    | "paste." `isPrefixOf` host = pasteHandler
    | otherwise                  = seeOther "http://www.n-sch.de" $ toResponse "File not found. Redirecting to n-sch.de"
    -- | otherwise                  = notFound . toResponse $ "404: File not found."
