{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}
module Paste.StateOld where

import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime

import Users.State (User (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        newtype ID = ID { unId :: String }

        -- | Way content is saved: either in a file or plain as a string
        data Content = File { filepath :: String }
                     | Plain { plain   :: String }

        -- | PasteEntry: Simple paste entry
        data PasteEntry = PasteEntry
                    { user      :: Maybe User
                    , pId       :: Maybe ID
                    , date      :: Maybe ClockTime
                    , content   :: Content
                    }

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries  :: [PasteEntry]
                           , lastID        :: Maybe ID
                           }
  
    |])

-- Add missing instances
$(deriveSerialize ''ID)
instance Version ID
$(deriveSerialize ''Content)
instance Version Content
$(deriveSerialize ''PasteEntry)
instance Version PasteEntry where
    mode = Versioned 1 Nothing   -- extension 1 (Proxy :: Proxy Old.PasteEntry)
$(deriveSerialize ''Paste)
instance Version Paste

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] Nothing

