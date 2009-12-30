{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Paste ( Paste(..) ) where

import qualified Data.Map as M

import Happstack.Data
import Happstack.Server.HTTP.Types  (Host)
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))

import Paste.State.ID               (ID (..))
import Paste.State.PasteEntry       (PasteEntry (..))

-- For migration:
import qualified Paste.State.Old.Paste2 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries       :: M.Map ID PasteEntry
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
    migrate (Old.Paste entries ids hosts) = Paste (M.fromList $ map (\entry -> (pId entry, entry)) entries)
                                                  (M.fromListWith (++) $ map (\(ct,h) -> (h,[ct])) hosts)
