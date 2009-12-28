
module Paste.Types.Status
    ( LoggedIn (..)
    ) where

import Users.State (User)

data LoggedIn = LoggedInAs User
              | NotLoggedIn
