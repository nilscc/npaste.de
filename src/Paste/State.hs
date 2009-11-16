{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State
    ( AddPaste (..)
    , GetAllEntries (..)
    , GetPasteById (..)
    , GenerateId (..)
    , GetPastesByUser (..)
    , GetPasteEntryByMd5sum (..)
    , defaultId
    , defaultIds
    , randomId
    , randomIds
    , tinyIds
    , customId
    , md5string

    , module Paste.State.Content
    , module Paste.State.ID
    , module Paste.State.IDType
    , module Paste.State.Paste
    , module Paste.State.PasteEntry
    )
    where

--------------------------------------------------------------------------------
-- State imports
--------------------------------------------------------------------------------

import Paste.State.Content
import Paste.State.ID
import Paste.State.IDType
import Paste.State.Paste
import Paste.State.PasteEntry




import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime
import Happstack.Crypto.MD5 (md5)

import System.Random
import System.Time

import Control.Monad (liftM2, forM)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Typeable (Typeable)
import Data.List (find, (\\), null, group)
import Data.Maybe (fromJust, fromMaybe)

import Users.State (User (..))


-- | Reserved IDs
validChars  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

reservedIds = [ "static", "client" ] ++ defaultIds ++ randomIds ++ tinyIds
defaultIds  = [ "", "default", "default id", "defaultid" ]
randomIds   = [ "rand", "random", "random id", "randomid" ]
tinyIds     = [ "tiny", "tiny url", "tinyurl" ]

-- | Generate MD5 sum of a string, returns a strict ByteString
md5string :: String -> BS.ByteString
md5string str = BS.concat . BS8.toChunks . md5 $ BS8.pack str


--------------------------------------------------------------------------------
-- State functions
--------------------------------------------------------------------------------


-- | Get a paste by ID
getPasteById :: ID -> Query Paste (Maybe PasteEntry)
getPasteById id = ask >>= return . find ((== id) . pId) . pasteEntries

-- | Get all pastes of a user
getPastesByUser :: User -> Query Paste [PasteEntry]
getPastesByUser u = ask >>= return . filter ((== Just u) . user) . pasteEntries

-- | Get all entries
getAllEntries :: Query Paste [PasteEntry]
getAllEntries = ask >>= return . pasteEntries

-- | Return ID of an MD5 sum
getPasteEntryByMd5sum :: BS.ByteString -> Query Paste (Maybe PasteEntry)
getPasteEntryByMd5sum bs = ask >>= return . find ((== bs) . md5hash) . pasteEntries




--------------------------------------------------------------------------------
-- State: ID functions
--------------------------------------------------------------------------------


-- | General ID generation
generateId :: IDType -> Query Paste ID
generateId idT = do
    id <- generateId' idT
    -- Limit ID size to 15 chars
    return $ case id of
                  ID s | length s <= 15 -> ID s
                  _                     -> NoID

generateId' DefaultID    = defaultId
generateId' (RandomID r) = randomId (r,r)
generateId' (CustomID i) = customId i


-- | Generate a new default ID
defaultId :: Query Paste ID
defaultId = do
    (Paste _ ids) <- ask
    let ids' = map unId $ filter isId ids
    return . ID $ defaultId' ids'

  where isId (ID _) = True
        isId NoID   = False

-- Helper for defaultId
defaultId' :: [String] -> String
defaultId' ids = head $ dropWhile (`elem` ids ++ reservedIds) everything

  where everything = concat $ iterate func chars
        func list  = concatMap (\char -> map (char ++) list) chars
        chars      = map (:[]) validChars


-- | Random ID generation. Increases maximum ID length everytime it fails to
-- create a unique ID.
randomId :: (Int,Int) -- ^ min & max number of chars to start with
         -> Query Paste ID
randomId r@(min,max) = do
    paste <- ask
    n     <- getRandomR r
    iList <- randomRs n (0,length validChars - 1) []

    let randId = ID $ map (validChars !!) iList

    if randId `elem` pasteIDs paste
       then randomId (min,max+1) -- increase max number to make sure we don't run out of IDs
       else return randId

  where randomRs 0 _ akk = return akk
        randomRs n r akk = do random <- getRandomR r
                              randomRs (n-1) r (akk ++ [random])

-- | Validate a custom ID
customId :: ID -> Query Paste ID
customId id@(ID id') = do
    paste <- ask
    let ids = reservedIds ++ (map unId $ pasteIDs paste)
    if not (id' `elem` ids) && all (`elem` validChars) id'
       then return id
       else return NoID




--------------------------------------------------------------------------------
-- State: Change content
--------------------------------------------------------------------------------


-- | Add a PasteEntry, returns the ID of the new paste
addPaste :: PasteEntry -> Update Paste ID
addPaste entry = do

    -- get paste
    paste <- ask
    -- get time
    ctime <- getEventClockTime

    let id      = pId entry
        ids     = pasteIDs paste
        entries = pasteEntries paste
    case id of
         ID _ | not (id `elem` ids) -> do
             modify $ \paste -> paste { pasteEntries = entry { date = ctime } : entries
                                      , pasteIDs     = id : ids
                                      }
             return id
         _ -> return NoID




-- Generate methods
$(mkMethods ''Paste ['addPaste, 'getPastesByUser, 'getPasteById, 'getPasteEntryByMd5sum, 'generateId, 'getAllEntries])
