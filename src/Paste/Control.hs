module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Control.Monad (msum, unless, when)
import Control.Monad.Trans (liftIO)
import System.Directory

import Paste.View (showPaste)
import Paste.Post (postHandler)

import Paste.State
    ( DeleteAllPastes(..)
    , AddPaste(..)
    , PasteEntry(..)
    , ID(..)
    , Content(..)
    , GetAllEntries(..)
    )
import System.Time (getClockTime, ClockTime(..))

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    , dir "restore" restore
    , dir "static" $ fileServe [] "npaste.de"
    , path showPaste
    , fileServe ["index.html"] "npaste.de"
    ]



-- | Restore lost data
restore = do
    let fp = "pastes"
    pastes <- liftIO $ getDirectoryContents fp >>= return . filter (all (`elem` chars))
    -- delete ALL pastes from memory! carefull
    update $ DeleteAllPastes
    -- add new pastes
    now <- liftIO getClockTime
    mapM_ (addPaste now) pastes

    -- show current pasteentrys
    pEntries <- query GetAllEntries
    ok . toResponse $ show pEntries

  where chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        addPaste now s = update $ AddPaste PasteEntry { date     = now
                                                      , content  = File ("pastes/" ++ s)
                                                      , user     = Nothing
                                                      , pId      = NoID
                                                      , filetype = Nothing
                                                      }
