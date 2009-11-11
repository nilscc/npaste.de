{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State
    ( Paste (..)
    , PasteEntry (..)
    , ID (..)
    , Content (..)
    , AddPaste (..)
    , GetPasteById (..)
    , GetNewId (..)
    , GetPastesByUser (..)
    )
    where

import Users.State (User (..))
import qualified Paste.StateOld as Old

import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime

import System.Time

import Control.Monad (liftM2)
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import Data.Typeable (Typeable)
import Data.List (find, (\\))
import Data.Maybe (fromJust, fromMaybe)


-- {{{ Data definitions
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        data ID = ID { unId :: String }
                | NoID

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

-- Add missing instances
$(deriveSerialize ''ID)
instance Version ID where
    mode = extension 1 (Proxy :: Proxy Old.ID)
$(deriveSerialize ''Content)
instance Version Content
$(deriveSerialize ''PasteEntry)
instance Version PasteEntry where
    -- mode = Versioned 1 Nothing
    mode = extension 1 (Proxy :: Proxy Old.PasteEntry)
$(deriveSerialize ''Paste)
instance Version Paste where
    mode = extension 1 (Proxy :: Proxy Old.Paste)

-- Make Paste its own Component
instance Component Paste where
  type Dependencies Paste = End
  initialValue = Paste [] []

instance Migrate Old.ID ID where
    migrate old = ID . Old.unId $ old
-- Migrate from older Versions of PasteEntry
instance Migrate Old.PasteEntry PasteEntry where
    migrate old = PasteEntry { user     = Nothing
                             , pId      = maybe NoID (migrate) (Old.pId old)
                             , date     = maybe (error "Invalid past entry: no paste date")
                                                (id)
                                                (Old.date old)
                             , content  = case Old.content old of
                                               Old.File fp    -> File fp
                                               Old.Plain text -> Plain text
                             , filetype = Nothing
                             }

instance Migrate Old.Paste Paste where
    migrate old = Paste { pasteEntries = map migrate $ Old.pasteEntries old
                        , pasteIDs = map (pId . migrate) $ Old.pasteEntries old
                        }

-- }}} Data definitions


-- {{{ Pure

-- Generate a new ID
incId :: Paste -> ID
incId (Paste _ ids) = incId' ids' id

  where isId (ID _) = True
        isId NoID   = False
        ids'        = filter isId ids
        id | null ids' = ""
           | otherwise = unId $ last ids'

-- Helper for incId
incId' ids id
    | all (== 'z') id || id == "" = ID $ head chars : map (const $ head chars) id
    | otherwise = ID . head . tail . takeWhile (`elem` ids') . everything $ length id

  where chars   = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        ids' = map unId ids
        everything n
            | n <= 1    = map (\c -> [c]) chars
            | otherwise = do
                c <- chars
                r <- everything (n-1)
                return $ c : r

-- }}} Pure


-- | Get a paste by ID
getPasteById :: ID -> Query Paste (Maybe PasteEntry)
getPasteById id = ask >>= return . find ((== id) . pId) . pasteEntries

-- | Get all pastes of a user
getPastesByUser :: User -> Query Paste [PasteEntry]
getPastesByUser u = ask >>= return . filter ((== Just u) . user) . pasteEntries

-- | Get a new ID
getNewId :: Query Paste ID
getNewId = ask >>= return . incId

-- | Add a PasteEntry, returns the ID of the new paste
addPaste :: PasteEntry -> Update Paste ID
addPaste entry = do
    paste <- ask
    let newId = incId paste
    modify $ \paste -> paste { pasteEntries = entry { pId = newId } : pasteEntries paste
                             , pasteIDs = newId : pasteIDs paste
                             }
    return newId

$(mkMethods ''Paste ['addPaste, 'getPastesByUser, 'getPasteById, 'getNewId])
