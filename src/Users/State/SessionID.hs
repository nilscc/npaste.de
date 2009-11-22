{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.SessionID ( SessionID (..) ) where

import Happstack.Data
import Happstack.Server.HTTP.Types (Host)

$(deriveAll [''Show, ''Eq, ''Ord, ''Read, ''Default]
  [d|

        -- | Session ID
        newtype SessionID = SessionID { sessionID   :: Integer }

  |])

$(deriveSerialize ''SessionID)
instance Version SessionID
