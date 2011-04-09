{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.NewTypes
    ( PUser (..)
    , module Paste.State.Old.NewTypes0
    ) where

import Happstack.Data

import qualified Happstack.Auth  as Auth

-- Reimport the old states
import Paste.State.Old.NewTypes0 hiding (PUser (..))

-- Migrate old PUser...
import qualified Paste.State.Old.NewTypes0 as Old

deriveAll [''Show, ''Eq, ''Ord] [d|

  newtype PUser = PUser { unUser :: (Maybe Auth.UserId) }

  |]

-- newtype stuff
deriveSerialize ''PUser

instance Version PUser where
    mode = extension 1 (Proxy :: Proxy Old.PUser)

instance Migrate Old.PUser PUser where
    migrate (Old.PUser _) = PUser Nothing -- get rid of everything old here :)
