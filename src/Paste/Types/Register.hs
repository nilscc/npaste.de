module Paste.Types.Register
    ( RegisterError (..)
    ) where

import Control.Monad.Error (Error (..))

data RegisterError = InvalidName
                   | InvalidEmail
                   | OtherRegisterError String

instance Show RegisterError where
    show InvalidName    = "Invalid name."
    show InvalidEmail   = "Invalid email."
    show (OtherRegisterError s)      = s

instance Error RegisterError where
    strMsg = OtherRegisterError
