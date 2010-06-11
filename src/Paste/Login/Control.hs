module Paste.Login.Control
    ( loginControl
    , logoutControl
    ) where

import Control.Monad
import Happstack.Server

import Paste.Login.AlreadyLoggedIn
import Paste.Login.Login
import Paste.Login.Logout
import Util.Control

loginControl :: ServerPart Response
loginControl = msum

    [ withoutLogin $ msum
        [ loginPerform
        , loginMain
        ]

    , withLogin alreadLoggedIn
    ]

logoutControl :: ServerPart Response
logoutControl = withLogin $ msum

    [ logoutPerform
    ]
