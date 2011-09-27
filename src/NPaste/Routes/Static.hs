module NPaste.Routes.Static where

import Happstack.Server
import NPaste.Types

staticR :: ServerPart Response
staticR = msum
  [ dir "css" $ serveDirectory DisableBrowsing [] "htdocs/css"
  , dir "js"  $ serveDirectory DisableBrowsing [] "htdocs/js"
  ]
