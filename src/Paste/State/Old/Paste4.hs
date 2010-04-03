{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.Paste4 ( Paste(..) ) where

import qualified Data.Map as M

import Happstack.Data
import Happstack.Server.HTTP.Types  (Host)
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))
import Happstack.Data.IxSet

import Paste.State.ID               (ID (..))
import Paste.State.PasteDB

-- For migration:
import qualified Paste.State.Old.Paste3 as Old
import qualified Paste.State.Old.PasteEntry6 as OldPE

$(deriveAll [''Show, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteDB            :: PasteDB
                           , knownHosts         :: M.Map Host [ClockTime]
                           , replies            :: M.Map ID [ID]
                           }

    |])

$(deriveSerialize ''Paste)
instance Version Paste where
    mode = extension 4 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste empty M.empty M.empty


instance Migrate Old.Paste Paste where
    migrate (Old.Paste entries hosts) =
        Paste (foldr insert empty . map migrate $ M.elems entries)
              hosts
              (M.fromList . map (\entry -> (OldPE.pId entry, OldPE.responses entry)) $ M.elems entries)
