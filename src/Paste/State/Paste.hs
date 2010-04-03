{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Paste ( Paste(..) ) where

import qualified Data.Map as M

import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))
import Happstack.Data.IxSet

import Paste.State.ID               (ID (..))
import Paste.State.PasteDB

-- For migration:
import qualified Paste.State.Old.Paste4 as Old

type Hostname = String

$(deriveAll [''Show, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteDB            :: PasteDB
                           , knownHosts         :: M.Map Hostname [ClockTime]
                           , replies            :: M.Map ID [ID]
                           }

    |])

$(deriveSerialize ''Paste)
instance Version Paste where
    mode = extension 5 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste empty M.empty M.empty


instance Migrate Old.Paste Paste where
    migrate (Old.Paste db entries hosts) =
        Paste db
              (M.mapKeys (\(h,_) -> h) entries)
              hosts
