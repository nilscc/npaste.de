{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State1
    ( Paste (..)
    , PasteEntry (..)
    , ID (..)
    , User (..)
    , Content (..)
    , AddPaste (..)
    , GetPasteById (..)
    , GetNewId (..)
    , GetPastesByUser (..)
    )
    where

import Happstack.Data (deriveAll, Default(..))
import Happstack.State
import Happstack.State.ClockTime

import System.Time

import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import Data.Typeable (Typeable)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)


-- {{{ Data definitions
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        newtype User = User { unUser :: String }
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
$(deriveSerialize ''User)
instance Version User
$(deriveSerialize ''Content)
instance Version Content
$(deriveSerialize ''PasteEntry)
instance Version PasteEntry
$(deriveSerialize ''Paste)
instance Version Paste

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] Nothing

-- }}} Data definitions


-- {{{ Pure

-- Generate a new ID
incId :: ID -> ID
incId (ID id)
    | all (== 'z') id || id == "" = ID $ head chars : map (const $ head chars) id
    | otherwise = ID . head . tail . dropWhile (/= id) . everything $ length id

  where chars   = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        everything n
            | n <= 1    = map (\c -> [c]) chars
            | otherwise = do
                c <- chars
                r <- everything (n-1)
                return $ c : r

-- }}} Pure


-- | Get a paste by ID
getPasteById :: ID -> Query Paste (Maybe PasteEntry)
getPasteById id = ask >>= return . find ((== id) . fromJust . pId) . pasteEntries

-- | Get all pastes of a user
getPastesByUser :: User -> Query Paste [PasteEntry]
getPastesByUser u = ask >>= return . filter ((== Just u) . user) . pasteEntries

-- | Get a new ID
getNewId :: Query Paste ID
getNewId = ask >>= return . incId . fromMaybe (ID "") . lastID

-- | Add a PasteEntry, returns the ID of the new paste
addPaste :: PasteEntry -> Update Paste ID
addPaste entry = do
    paste <- ask
    let newId = incId . fromMaybe (ID "") . lastID $ paste
    modify $ \paste -> Paste { lastID = Just newId
                             , pasteEntries = entry { pId = Just newId } : pasteEntries paste
                             }
    return newId

$(mkMethods ''Paste ['addPaste, 'getPastesByUser, 'getPasteById, 'getNewId])
