{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Paste ( Paste(..) ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))
import Happstack.Data.IxSet

import Paste.State.ID               (ID (..))
import Paste.State.PasteDB

-- For migration:
import qualified Paste.State.Old.Paste5 as Old

type Hostname = String

deriveAll [''Show] [d|

  -- | Paste: A list of all PasteEntry with the last used ID
  data Paste = Paste
    { pasteDB            :: PasteDB
    , knownHosts         :: M.Map Hostname [ClockTime]
    , replies            :: M.Map ID [ID]
    , removedIds         :: S.Set ID
    }

  |]

deriveSerialize ''Paste

instance Version Paste where
    mode = extension 6 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste empty M.empty M.empty S.empty


instance Migrate Old.Paste Paste where
    migrate (Old.Paste db hosts replies) =
        Paste db hosts replies S.empty
