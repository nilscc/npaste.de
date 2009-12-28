module Paste.Post.Register
    ( register
    ) where

import Control.Monad.Error          (ErrorT (..), throwError)
import Happstack.Server             (ServerPartT (..), Response)

import Paste.Types.Register         (RegisterError (..))

type Nick = String

register :: ErrorT RegisterError (ServerPartT IO) Nick
register = throwError $ OtherRegisterError "Not implemented yet."
