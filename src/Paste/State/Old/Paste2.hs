{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.Paste2 ( Paste(..) ) where


import Happstack.Data
import Happstack.Server.HTTP.Types  (Host)
import Happstack.State
import Happstack.State.ClockTime    (ClockTime (..))

import Paste.State.ID               (ID (..))
import Paste.State.PasteEntry       (PasteEntry (..))

-- For migration:
import qualified Paste.State.Old.Paste1 as Old
import qualified Paste.State.Old.PasteEntry6 as OldPE (PasteEntry (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries  :: [OldPE.PasteEntry]
                           , pasteIDs      :: [ID]
                           , knownHosts    :: [(ClockTime,Host)]
                           }

    |])


$(deriveSerialize ''Paste)
instance Version Paste where
    mode = extension 2 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] [] []


instance Migrate Old.Paste Paste where
    migrate (Old.Paste entries ids) = Paste entries ids []
