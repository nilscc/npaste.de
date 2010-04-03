
module Paste.Types.Status
    ( LoggedIn (..)
    ) where

import Happstack.Auth (SessionKey)

data LoggedIn = LoggedInAs SessionKey
              | NotLoggedIn
              deriving (Eq, Show, Ord)
