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
    , GetAllIds (..)
    , GetPasteById (..)
    , GenerateId (..)
    , GetPastesByUser (..)
    , GetPasteEntryByMd5sum (..)

    , AddResponse (..)
    , GetAllReplies (..)

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
    , module Paste.State.NewTypes
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
import Paste.State.NewTypes


import Happstack.Data
import Happstack.Data.IxSet
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
import Data.Maybe                   (fromJust, fromMaybe)
import qualified Data.List as L
import qualified Data.Set  as S
import qualified Data.Map  as M

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import Users.State
    ( User (..)
    , Password
    , PasswordPlain
    , Login
    )


-- | Reserved IDs
validChars  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

reservedIds = [ "static", "client" ] ++ defaultIds ++ randomIds ++ tinyIds ++ restrictedIds
defaultIds  = [ "", "default", "default id", "defaultid" ]
randomIds   = [ "rand", "random", "random id", "randomid" ]
tinyIds     = [ "tiny", "tiny url", "tinyurl" ]

restrictedIds = [ "nazi", "hitler" ]


md5string :: String -> BS.ByteString
md5string = BS.concat . BS8.toChunks . md5 . BS8.pack

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------


-- | Get a paste by ID
getPasteById :: ID -> Query Paste (Maybe PasteEntry)
getPasteById id = ask >>= return . getOne . (@= PId id)  . pasteDB

-- | Get all pastes of a user
getPastesByUser :: Maybe User -> Query Paste (S.Set PasteEntry)
getPastesByUser u = ask >>= return . toSet . (@= PUser u) . pasteDB

-- | Get all entries
getAllEntries :: Query Paste (S.Set PasteEntry)
getAllEntries = ask >>= return . toSet . pasteDB

-- | Return ID of an MD5 sum
getPasteEntryByMd5sum :: Maybe User -> BS.ByteString -> Query Paste (Maybe PasteEntry)
getPasteEntryByMd5sum u md = ask >>= \Paste { pasteDB = ix } ->
    return . getOne $ ix @= (PUser u) @= (PHash md)

-- | Return all IDs
getAllIds :: Query Paste (S.Set ID)
getAllIds = getAllEntries >>= return . S.map (unPId . pId)




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
    ids <- getAllIds
    return . ID . head $ dropWhile (\id -> id `elem` reservedIds || (ID id) `S.member` ids) everything

  where isId (ID _) = True
        isId NoID   = False
        everything = concat $ iterate func chars
        func list  = concatMap (\char -> map (char ++) list) chars
        chars      = map (:[]) validChars


-- | Random ID generation. Increases maximum ID length everytime it fails to
-- create a unique ID.
randomId :: Maybe User            -- ^ user posting that paste
         -> (Int,Int)       -- ^ min & max number of chars to start with
         -> Query Paste ID
randomId us r@(min,max) = do
    ids   <- getAllIds
    n     <- getRandomR r
    iList <- randomRs n (0,length validChars - 1) []

    let randId = ID $ map (validChars !!) iList

    if (randId `S.member` ids)
       then randomId us (min,max+1) -- increase max number to make sure we don't run out of IDs
       else return randId

  where randomRs 0 _ akk = return akk
        randomRs n r akk = do random <- getRandomR r
                              randomRs (n-1) r (akk ++ [random])

-- | Validate a custom ID
customId :: Maybe User -> ID -> Query Paste ID
customId us id@(ID id') = do
    ids <- getAllIds
    if  all (`elem` validChars) id' && not (id' `elem` reservedIds || id `S.member` ids)
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

    modify $ \paste -> paste { knownHosts = M.insertWith (++) host [ctime] $ knownHosts paste }

-- | Remove all known hosts older than X minutes
clearKnownHosts :: Int              -- ^ age in minutes (one month = 30 days)
                -> Update Paste ()
clearKnownHosts min = do
    paste <- ask
    ctime <- getEventClockTime
    let maxTime = normalizeTimeDiff $ noTimeDiff { tdMin = min }
        timeDiff time = let tdiff = normalizeTimeDiff $ diffClockTimes ctime time
                        in tdiff <= maxTime
        clear times = case filter timeDiff times of
                           [] -> Nothing
                           l  -> Just l
    modify $ \paste -> paste { knownHosts = M.mapMaybe clear $ knownHosts paste }

-- | Get ClockTime if pasteNo >= maxNum
getClockTimeByHost :: Int                            -- ^ max number of pastes
                   -> Host                           -- ^ host
                   -> Query Paste (Maybe ClockTime)
getClockTimeByHost maxNum host = do
    -- get paste & current time
    paste <- ask
    ctime <- getEventClockTime

    -- get oldest paste time
    let ctimes = fromMaybe [] . M.lookup host $ knownHosts paste

    return $ if length ctimes >= maxNum
                then Just $ minimum ctimes
                else Nothing


--------------------------------------------------------------------------------
-- Add / remove / change pastes
--------------------------------------------------------------------------------

-- | Add a PasteEntry, returns the ID of the new paste
addPaste :: PasteEntry -> Update Paste ID
addPaste entry = do

    -- get entries
    ids <- runQuery $ getAllIds
    -- get time
    ctime <- getEventClockTime

    let pid = pId entry
    case pid of
         PId id | not (id `S.member` ids) -> do
             modify $ \paste -> paste { pasteDB = insert entry $ pasteDB paste }
             return id
         _ -> return NoID


--------------------------------------------------------------------------------
-- Description stuff (Twitter style yay!)
--------------------------------------------------------------------------------

-- | Add a response from ID to the paste with ID
addResponse :: ID               -- ^ ID of the response
            -> ID               -- ^ ID of the paste that gets the response
            -> Update Paste ()
addResponse from to = modify $ \paste ->
    if Happstack.Data.IxSet.null $ (pasteDB paste) @= PId to
       then paste
       else paste { replies = M.alter addFrom to $ replies paste }

  where addFrom (Just list) = Just . L.nub $ L.insert from list
        addFrom _           = Just [from]

-- | Get all responses of ID
getAllReplies :: ID -> Query Paste [ID]
getAllReplies id = ask >>= return . M.findWithDefault [] id . replies



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
    , 'getAllIds
    , 'getAllReplies
    , 'addResponse
    ])
