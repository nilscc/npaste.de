{-# LANGUAGE OverloadedStrings #-}

module NPaste.Routes
  ( npasteR
  ) where

import Data.Time
import Happstack.Server

import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import Text.Blaze ((!))

import NPaste.Database
import NPaste.Types
import NPaste.State

import NPaste.Routes.Index
import NPaste.Routes.Partial
import NPaste.Routes.Read
import NPaste.Routes.Static
import NPaste.Routes.View

import NPaste.Html.About

npasteR :: NPaste ()
npasteR = choice
  [ nullDir >> indexR
  -- , dir "f" findR
  , dir "v" viewR
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
  u        <- getNumberOfUsers
  p        <- getNumberOfPastes
  now      <- liftIO getCurrentTime
  HtmlBody .= aboutHtml u p now
