module NPaste.Routes.Static where

import Happstack.Server
import NPaste.Types
import NPaste.State

staticR :: NPaste ()
staticR = choice
  [ dir "css" $ setNP . PlainResponse =<< serveDirectory DisableBrowsing [] "htdocs/css"
  , dir "js"  $ setNP . PlainResponse =<< serveDirectory DisableBrowsing [] "htdocs/js"
  ]
