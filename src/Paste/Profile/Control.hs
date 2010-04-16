module Paste.Profile.Control
    ( profileControl
    ) where

import Control.Monad
import Happstack.Server

import Paste.Profile.Profile

profileControl :: ServerPart Response
profileControl = msum

    [ profileUpdate
    , profileActivateEmail
    , profileCancelActivation
    , profileShow
    ]
