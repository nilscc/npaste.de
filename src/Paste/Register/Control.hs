module Paste.Register.Control
    ( registerControl
    ) where

import Control.Monad
import Happstack.Server
import Paste.Register.Register

import Paste.Login.AlreadyLoggedIn
import Util.Control

registerControl :: ServerPart Response
registerControl = msum

    [ withoutLogin $ msum

        [ methodM POST >> registerNew
        , methodM GET  >> registerActivate
        , registerMain
        ]

    , withLogin alreadLoggedIn
    ]
