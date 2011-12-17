{-# LANGUAGE OverloadedStrings #-}

module NPaste.Routes
  ( npasteR
  ) where

import Happstack.Server

import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import Text.Blaze ((!))

import NPaste.Database
import NPaste.Types
import NPaste.State

import NPaste.Routes.Find
import NPaste.Routes.Index
import NPaste.Routes.Partial
import NPaste.Routes.Read
import NPaste.Routes.Static

import NPaste.Html.About

npasteR :: NPaste ()
npasteR = choice
  [ nullDir >> indexR
  , dir "f" findR
  , dir "t" tagR
  , dir "a" aboutR
  , dir "p" partialR
  , dir "s" staticR
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

aboutR :: NPaste ()
aboutR = do
  setNP M_About
  CSS      .= ["document.css"]
  Title    .= Just "About"
  u        <- numberOfUsers
  p        <- numberOfPastes
  HtmlBody .= aboutHtml u p
