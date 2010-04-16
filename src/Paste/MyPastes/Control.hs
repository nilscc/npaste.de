module Paste.MyPastes.Control
    ( myPastesControl
    ) where

import Control.Monad
import Happstack.Server
import Paste.MyPastes.View

myPastesControl :: ServerPart Response
myPastesControl = msum

    -- [ methodM POST >> 
    [ showMyPastes
    ]
