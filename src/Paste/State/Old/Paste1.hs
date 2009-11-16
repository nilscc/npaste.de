{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.Paste1 ( Paste(..) ) where


import Happstack.Data
import Happstack.State

import Paste.State.ID           (ID (..))
import Paste.State.PasteEntry   (PasteEntry (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries  :: [PasteEntry]
                           , pasteIDs      :: [ID]
                           -- , knownHosts    :: [(ClockTime,Host)]
                           }

    |])


$(deriveSerialize ''Paste)
instance Version Paste where
    mode = Versioned 1 Nothing

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] []
