module Paste.MyPastes.Control
    ( myPastesControl
    ) where

import Control.Monad
import Happstack.Server
import Paste.MyPastes.View
import Paste.MyPastes.Edit
import Paste.MyPastes.Remove

myPastesControl :: ServerPart Response
myPastesControl = msum

    [ editMyPaste
    , removeMyPaste
    , showMyPastes
    ]
