{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.Login.AlreadyLoggedIn
    ( alreadLoggedIn
    ) where

import HSP
import Happstack.Server

import Paste.View

alreadLoggedIn :: ServerPart Response
alreadLoggedIn = htmlBody

    [ 
        <div id="main">
            <h1>Error</h1>
            <p class="error">You are already logged in!</p>
        </div>
    ]
