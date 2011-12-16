module NPaste.Routes.Static where

import Happstack.Server
import NPaste.Types

staticR :: NPaste ()
staticR = do
  msum
    [ dir "css" $ setNP . PlainResponse =<< serveDirectory DisableBrowsing [] "htdocs/css"
    , dir "js"  $ setNP . PlainResponse =<< serveDirectory DisableBrowsing [] "htdocs/js"
    ]
