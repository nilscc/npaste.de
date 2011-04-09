{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.UserData0 ( UserData (..) ) where

import Happstack.Data

deriveAll [''Show, ''Eq, ''Ord] [d|

  -- | All informations about our user
  data UserData = UserData
    { userEmail         :: String       -- ^ Email
    }

  |]

deriveSerialize ''UserData
instance Version UserData
