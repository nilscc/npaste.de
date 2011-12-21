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

import NPaste.Routes.About
import NPaste.Routes.Index
import NPaste.Routes.Partial
import NPaste.Routes.Read
import NPaste.Routes.Static
import NPaste.Routes.User
import NPaste.Routes.View

npasteR :: NPaste ()
npasteR = choice
  [ nullDir >> indexR
  , dir "a" aboutR
  , dir "p" partialR
  , dir "s" staticR
  , dir "u" userR
  , dir "v" viewR
  , path readR
  , notFoundR
  ]

--------------------------------------------------------------------------------
-- Simple routes

notFoundR :: NPaste ()
notFoundR = do
  ResponseCode .= notFound
  Title        .= Just "Page not found"
  HtmlBody     .= H.h2 ! A.class_ "error" $ "The page you requested was not found."
