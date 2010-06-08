{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Info
    ( showInfo
    ) where

import Control.Monad.Trans      (liftIO)

import qualified Data.Set as S
import qualified Data.Map as M

import HSP
import Happstack.Data.IxSet (size)
import Happstack.Server
import Happstack.State
import qualified Happstack.Auth as Auth

import System.Time

import Paste.View
import Paste.State
import Users.State

showInfo :: ServerPart Response
showInfo = do

    now         <- liftIO getClockTime
    update $ RemoveInactiveUsers

    pentries    <- query GetAllEntries
    userdb      <- query Auth.AskUsers
    users       <- query AskUsers

    let info = [ Info "Total number of pastes"  $ show (S.size pentries)
               , Info "Registered users"        $ show (size userdb)
               , Info "Inactive users"          $ show (M.size $ inactiveUsers users)
               ]

    htmlBody [infoHsp now info]


data Info = Info { infoKey :: String
                 , infoVal :: String
                 }


--------------------------------------------------------------------------------
-- HSP definition
--------------------------------------------------------------------------------

infoHsp :: ClockTime -> [Info] -> HSP XML
infoHsp date infos =
    <div id="main">
        <h1>Status information</h1>
        <p>Current status:</p>
        <%
            if null infos
               then <p class="error">No information available.</p>
               else <ul id="info"><% infos %></ul>
        %>
        <p>State: <% calendarTimeToString . toUTCTime $ date %></p>

        <h1>Contact information</h1>
        <p>You can contact me via email, jabber or on irc:</p>
        <ul>
            <li>Email: mail [at] npaste [dot] de</li>
            <li>Jabber: mcmaniac [at] n-sch [dot] de</li>
            <li>IRC: McManiaC on Freenode</li>
            <li>Follow me and send me patches on <a href="http://github.com/mcmaniac/npaste.de">github.com</a>!</li>
        </ul>
    </div>


--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m Info) where
    asChild info = <% <li><p><span class="info-key"><% infoKey info %>:</span> <% infoVal info %></p></li> %>
