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

import NPaste.Routes.Find
import NPaste.Routes.Index
import NPaste.Routes.Read
import NPaste.Routes.Static

npasteR :: NPaste ()
npasteR = choice
  [ nullDir >> indexR
  , dir "f" findR
  , dir "t" tagR
  , dir "s" staticR
  , path readR
  , notFoundR
  ]

notFoundR :: NPaste ()
notFoundR = do
  ResponseCode .= (notFound :: Response -> ServerPart Response)
  Title        .= Just ("Page not found" :: String)
  HtmlBody     .= H.h2 ! A.class_ "error" $ "The page you requested was not found."
