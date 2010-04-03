module Paste.Register.Control
    ( registerControl
    ) where

import Control.Monad
import Happstack.Server
import Paste.Register.Register

registerControl :: ServerPart Response
registerControl = msum
    [ methodM POST >> registerNew
    , methodM GET  >> registerActivate
    , registerMain
    ]
