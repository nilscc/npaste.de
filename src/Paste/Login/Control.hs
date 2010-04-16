module Paste.Login.Control
    ( loginControl
    , logoutControl
    ) where

import Control.Monad
import Happstack.Server
import Paste.Login.Login
import Paste.Login.Logout

loginControl :: ServerPart Response
loginControl = msum

    [ loginPerform
    , loginMain
    ]

logoutControl :: ServerPart Response
logoutControl = msum

    [ logoutPerform
    ]
