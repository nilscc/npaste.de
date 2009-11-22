{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.User0 ( User (..) ) where

import Happstack.Data
import Users.State.Old.Login ( Login, Password )

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | User data
      data User = User { userLogin    :: Login
                       , userPassword :: Password
                       , userEmail    :: Maybe String -- ^ (optional) Email
                       }

  |])

$(deriveSerialize ''User)
instance Version User
