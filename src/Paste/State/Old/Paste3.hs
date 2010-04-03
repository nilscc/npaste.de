{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.Paste3 ( Paste(..) ) where

import qualified Data.Map as M

import Happstack.Data
import Happstack.Server.HTTP.Types  (Host)
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))

import Paste.State.ID               (ID (..))

-- For migration:
import qualified Paste.State.Old.Paste2 as Old
import qualified Paste.State.Old.PasteEntry6 as OldPE (PasteEntry (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries       :: M.Map ID OldPE.PasteEntry
                           , knownHosts         :: M.Map Host [ClockTime]
                           }

    |])


$(deriveSerialize ''Paste)
instance Version Paste where
    mode = extension 3 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste M.empty M.empty


instance Migrate Old.Paste Paste where
    migrate (Old.Paste entries _ hosts) = Paste (M.fromList $ map (\entry -> (OldPE.pId entry, entry)) entries)
                                                (M.fromListWith (++) $ map (\(ct,h) -> (h,[ct])) hosts)
