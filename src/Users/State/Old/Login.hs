{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.Login ( Login (..), Password (..) ) where

import Happstack.Data


$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | Login ID
      newtype Login     = Login    { login      :: String }
      -- | Password
      newtype Password  = Password { password   :: String }

  |])

$(deriveSerialize ''Login)
instance Version Login

$(deriveSerialize ''Password)
instance Version Password
