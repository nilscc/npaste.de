module NPaste.Routes.Static where

import Happstack.Server
import NPaste.Types
import NPaste.Utils

staticR :: NPaste ()
staticR = msum
  [ dir "css" $ do
      rq <- askRq
      PlainResponse rq .= serveDirectory DisableBrowsing [] "htdocs/css"
  , dir "js"  $do
      rq <- askRq
      PlainResponse rq .= serveDirectory DisableBrowsing [] "htdocs/js"
  ]
