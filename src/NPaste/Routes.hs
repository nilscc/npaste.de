{-# LANGUAGE OverloadedStrings #-}

module NPaste.Routes
  ( npasteR
  ) where

import Happstack.Server

import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import Text.Blaze ((!))

import NPaste.Types
import NPaste.State
import NPaste.Utils

import NPaste.Routes.About
import NPaste.Routes.Index
import NPaste.Routes.Partial
import NPaste.Routes.Read
import NPaste.Routes.Static
import NPaste.Routes.User
import NPaste.Routes.View

npasteR :: NPaste ()
npasteR = do
  setupUserMenu
  msum
    [ nullDir >> indexR
    , dir "a" aboutR
    , dir "j" partialR
    , dir "p" $ path readR
    , dir "s" staticR
    , dir "u" userR
    , dir "v" viewR
    , path robotsR
    , path readRedirectR
    , notFoundR
    ]

--------------------------------------------------------------------------------
-- Simple routes

robotsR :: String -> NPaste ()
robotsR "robots.txt" = do
  rq <- askRq
  PlainResponse rq .= serveFile (asContentType "text/plain") "htdocs/robots.txt"
robotsR _ = mzero

notFoundR :: NPaste ()
notFoundR = do
  ResponseCode .= notFound
  Title        .= Just "Page not found"
  ActiveMenu   .= Nothing
  HtmlBody     .= H.h2 ! A.class_ "error" $ "The page you requested was not found."
