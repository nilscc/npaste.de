{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.UserData ( UserData (..) ) where

import Happstack.Data
import Users.State.PasteSettings

import qualified Users.State.Old.UserData0 as Old

deriveAll [''Show, ''Eq, ''Ord] [d|

  -- | All informations about our user
  data UserData = UserData
    { userEmail             :: String                   -- ^ Current email
    , userEmailRequested    :: Maybe (String,String)    -- ^ Requested new email + activation key
    , defaultPasteSettings  :: PasteSettings
    }

  |]

deriveSerialize ''UserData

instance Version UserData where
    mode = extension 1 (Proxy :: Proxy Old.UserData)

instance Migrate Old.UserData UserData where
    migrate (Old.UserData email) = UserData email Nothing DefaultPasteSettings
