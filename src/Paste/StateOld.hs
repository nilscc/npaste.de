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

        -- | Default ID definition
        data ID = ID { unId :: String }
                | NoID

        -- | Data definitions for custom IDs
        data IDType = DefaultID     -- ^ generate default ID
                    | RandomID Int  -- ^ (min) number of digits
                    | CustomID ID   -- ^ custom ID

        -- | Way content is saved: either in a file or plain as a string
        data Content = File { filepath :: String }
                     | Plain { plain   :: String }

        -- | PasteEntry: Simple paste entry
        data PasteEntry = PasteEntry
                    { user      :: Maybe User
                    , pId       :: ID
                    , date      :: ClockTime
                    , content   :: Content
                    , filetype  :: Maybe String
                    }

        -- | Paste: A list of all PasteEntry with the last used ID
        data Paste = Paste { pasteEntries  :: [PasteEntry]
                           , pasteIDs      :: [ID]
                           }
  
    |])

$(deriveSerialize ''ID)
instance Version ID where
    mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.ID)

$(deriveSerialize ''IDType)
instance Version IDType -- where
    -- mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.ID)

$(deriveSerialize ''Content)
instance Version Content

$(deriveSerialize ''PasteEntry)
instance Version PasteEntry where
    mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.PasteEntry)

$(deriveSerialize ''Paste)
instance Version Paste where
    mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] []
