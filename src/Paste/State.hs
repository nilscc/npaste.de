{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State
    ( AddPaste (..)
    , AddKnownHost (..)
    , ClearKnownHosts (..)
    , GetClockTimeByHost (..)
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
import Happstack.Server.HTTP.Types  (Host)
import Happstack.State
import Happstack.State.ClockTime
import Happstack.Crypto.MD5         (md5)

import System.Random
import System.Time

import Control.Monad                (liftM2, forM)
import Control.Monad.Reader         (ask)
import Control.Monad.State          (modify)
import Control.Monad.Trans          (liftIO)

import Data.Typeable                (Typeable)
import Data.List                    (find, (\\), null, group)
import Data.Maybe                   (fromJust, fromMaybe)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

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
getPasteEntryByMd5sum :: Maybe User -> BS.ByteString -> Query Paste (Maybe PasteEntry)
getPasteEntryByMd5sum user' bs = ask >>= return . find userAndMd5 . pasteEntries
  where userAndMd5 pe = let peUser = user pe
                            peMd5  = md5hash pe
                        in user' == peUser && bs == peMd5




--------------------------------------------------------------------------------
-- State: ID functions
--------------------------------------------------------------------------------

-- TODO: Handle each users ID separately

-- | General ID generation
generateId :: Maybe User -> IDType -> Query Paste ID
generateId us idT = do
    id <- generateId' idT us
    -- Limit ID size to 15 chars
    return $ case id of
                  ID s | length s <= 15 -> ID s
                  _                     -> NoID

generateId' DefaultID    = defaultId
generateId' (RandomID r) = flip randomId (r,r)
generateId' (CustomID i) = flip customId i


-- | Generate a new default ID
defaultId :: Maybe User -> Query Paste ID
defaultId u = do
    (Paste _ ids _) <- ask
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
randomId :: Maybe User            -- ^ user posting that paste
         -> (Int,Int)       -- ^ min & max number of chars to start with
         -> Query Paste ID
randomId us r@(min,max) = do
    paste <- ask
    n     <- getRandomR r
    iList <- randomRs n (0,length validChars - 1) []

    let randId = ID $ map (validChars !!) iList

    if randId `elem` pasteIDs paste
       then randomId us (min,max+1) -- increase max number to make sure we don't run out of IDs
       else return randId

  where randomRs 0 _ akk = return akk
        randomRs n r akk = do random <- getRandomR r
                              randomRs (n-1) r (akk ++ [random])

-- | Validate a custom ID
customId :: Maybe User -> ID -> Query Paste ID
customId us id@(ID id') = do
    paste <- ask
    let ids = reservedIds ++ (map unId $ pasteIDs paste)
    if not (id' `elem` ids) && all (`elem` validChars) id'
       then return id
       else return NoID




--------------------------------------------------------------------------------
-- Paste operations
--------------------------------------------------------------------------------

-- | Add a host to the knownHosts field of our Paste
addKnownHost :: Host -> Update Paste ()
addKnownHost host = do

    paste <- ask
    ctime <- getEventClockTime

    modify $ \paste -> paste { knownHosts = (ctime,host) : knownHosts paste }

-- | Remove all known hosts older than X minutes
clearKnownHosts :: Int              -- ^ age in minutes (one month = 30 days)
                -> Update Paste ()
clearKnownHosts min = do
    paste <- ask
    ctime <- getEventClockTime
    let maxTime = normalizeTimeDiff $ noTimeDiff { tdMin = min }
        timeDiff (time,_) = let tdiff = normalizeTimeDiff $ diffClockTimes ctime time
                            in tdiff <= maxTime
    modify $ \paste -> paste { knownHosts = filter timeDiff $ knownHosts paste }

-- | Get ClockTime if pasteNo >= maxNum
getClockTimeByHost :: Int                            -- ^ max number of pastes
                   -> Host                           -- ^ host
                   -> Query Paste (Maybe ClockTime)
getClockTimeByHost maxNum host = do
    -- get paste & current time
    paste <- ask
    ctime <- getEventClockTime

    -- get oldest paste time
    let ctimes = filter ((== host) . snd) (knownHosts paste)

    if length ctimes >= maxNum
       then return . Just . minimum $ map fst ctimes
       else return Nothing

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
$(mkMethods ''Paste
    [ 'addPaste
    , 'addKnownHost
    , 'clearKnownHosts
    , 'getClockTimeByHost
    , 'getPastesByUser
    , 'getPasteById
    , 'getPasteEntryByMd5sum
    , 'generateId
    , 'getAllEntries
    ])
