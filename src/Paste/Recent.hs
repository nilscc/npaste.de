{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.Recent
    (
      RecentPaste (..)
    , makeRecent
    , getLatestPaste
    , getLatestPaste'
    , getRecentPastes
    , getRecentPastes'
    -- ^ Helping functions
    , sortByDate
    ) where

import Data.List                (sortBy)
import Control.Monad
import Control.Monad.Trans      (liftIO)
import Happstack.Server
import Happstack.State
import System.Time
import qualified Data.Set as S
import qualified Happstack.Auth      as Auth

import Paste.State
import Util.IO

--------------------------------------------------------------------------------
-- Data definitions + Helper stuff
--------------------------------------------------------------------------------

data RecentPaste = RecentPaste { rDate              :: ClockTime
                               , rCont              :: String
                               , rDesc              :: Maybe String
                               , rHide              :: Bool
                               , rFiletype          :: Maybe String
                               , rId                :: ID
                               , allIds             :: S.Set ID
                               }

-- | Turn a 'PasteEntry' into a 'RecentPaste', getting its date, content as a
-- String etc...
makeRecent :: Maybe Int                 -- ^ Content: Number of lines
           -> PasteEntry
           -> ServerPart RecentPaste
makeRecent n pe = do
    ids <- query $ GetAllIds
    content <- liftIO $ case unPContent (content pe) of
                             Plain str -> return str
                             File fp   -> readFile' fp
    return $ RecentPaste { rDate            = unPDate $ date pe
                         , rCont            = (maybe id (\n -> unlines . take n . lines) n) $ content
                         , rDesc            = unPDescription $ description pe
                         , rId              = unPId $ pId pe
                         , rHide            = unPHide $ hide pe
                         , rFiletype        = unPFileType $ filetype pe
                         , allIds           = ids
                         }


removeHidden :: S.Set PasteEntry -> S.Set PasteEntry
removeHidden = S.filter (not . unPHide . hide)

-- | Get all pastes of a user and turn them into recent pastes
getRecentPastes :: Maybe String                 -- ^ Username
                -> Int                         -- ^ Number of pastes to get
                -> Bool                         -- ^ Hide hidden pastes?
                -> Maybe Int                    -- ^ Content: Number of lines
                -> ServerPart [RecentPaste]
getRecentPastes u num h n = getRecentPastes' u num h >>= mapM (makeRecent n)

-- Variant
getRecentPastes' :: Maybe String                -- ^ Username
                 -> Int                         -- ^ Number of pastes to get
                 -> Bool                        -- ^ Hide hidden pastes?
                 -> ServerPart [PasteEntry]
getRecentPastes' (Just user) num h = msum
    [ do Auth.User { Auth.userid = uid } <- maybe mzero return =<< query (Auth.GetUser $ Auth.Username user)
         (take num . sortByDate . S.toList . if h then removeHidden else id) `fmap` query (GetPastesByUser (Just uid))
    , return []
    ]

getRecentPastes' _ num h = (take num . sortByDate . S.toList . if h then removeHidden else id) `fmap` query GetAllEntries


sortByDate :: [PasteEntry] -> [PasteEntry]
sortByDate = sortBy $ \p1 p2 -> (date p2) `compare` (date p1)


-- | Get latest paste of a user
getLatestPaste :: String        -- ^ Username
               -> Bool             -- ^ Hide hidden pastes?
               -> Maybe Int     -- ^ Content: Number of lines
               -> ServerPart (Maybe RecentPaste)
getLatestPaste u h n = do
    p <- getLatestPaste' u h
    case p of
         Just pe -> Just `fmap` makeRecent n pe
         _       -> return Nothing

getLatestPaste' :: String           -- ^ Username
                -> Bool             -- ^ Hide hidden pastes?
                -> ServerPart (Maybe PasteEntry)
getLatestPaste' user h = msum
    [ do

        Auth.User { Auth.userid = uid } <- maybe mzero return =<< query (Auth.GetUser $ Auth.Username user)
        pastes <- (sortByDate . S.toList) `fmap` query (GetPastesByUser (Just uid))

        case dropWhile (\PasteEntry { hide = PHide h' } -> h' == h) pastes of
             (p:_) -> return $ Just p
             _     -> mzero

    , return Nothing
    ]
