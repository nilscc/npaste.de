{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.SessionID ( SessionID (..) ) where

import Happstack.Data

$(deriveAll [''Show, ''Eq, ''Ord, ''Read, ''Default]
  [d|

        -- | Session ID
        newtype SessionID = SessionID { sessionID   :: Integer }

  |])

$(deriveSerialize ''SessionID)
instance Version SessionID
