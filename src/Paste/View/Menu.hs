{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Menu
    ( menuHsp
    ) where

import HSP
import Paste.Types.Status   (LoggedIn (..))


--------------------------------------------------------------------------------
-- Menu entries
--------------------------------------------------------------------------------

entries :: [Entry]
entries =
    [ Always        <a href="/">New paste</a>
    , Always        <a href="/?view=recent">View recent pastes</a>
    -- , WithLogin     <a href="/?view=my">View my pastes</a>
    -- , WithLogin     <a href="/?view=logout">Logout</a>
    -- , WithoutLogin  <p><a href="/?view=login">Login</a> or <a href="/?view=register">Register</a></p>
    , Always        <a href="/?view=download">Download clients</a>
    -- , Always        <p><a href="/?view=news">News</a>, <a href="/?view=info">Info</a> and <a href="/?view=faq">FAQ</a></p>
    , Always        <a href="/?view=faq">FAQ</a>
    ]




--------------------------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------------------------

type Url   = String
type Desc  = String
data Entry = Always       (HSP XML)
           | WithLogin    (HSP XML)
           | WithoutLogin (HSP XML)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Menu HSP
menuHsp :: LoggedIn -> HSP XML
menuHsp login =
    <ul id="menu">
        <% map makeLi $ withLogin login entries %>
    </ul>
  where makeLi entry = <li><% entry %></li>

-- Private helper: Filter menu entries
withLogin :: LoggedIn -> [Entry] -> [HSP XML]
withLogin NotLoggedIn entries = foldr filter [] entries
  where filter (Always xml) rest          = xml : rest
        filter (WithoutLogin xml) rest    = xml : rest
        filter _ rest                     = rest
