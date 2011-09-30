{-# LANGUAGE OverloadedStrings #-}

module NPaste.Routes
  ( npasteR
  ) where

import Happstack.Server

import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import Text.Blaze ((!))

import NPaste.Types
import NPaste.Routes.Index
import NPaste.Routes.Read
import NPaste.Routes.Static
import NPaste.Html

npasteR :: ServerPart Response
npasteR = msum
  [ nullDir >> indexR
  , path readR
  , dir "s" staticR
  , notFoundR
  ]

notFoundR :: ServerPart Response
notFoundR = do
  notFound . toResponse . mainFrame $ nullBody
    { html = H.h2 ! A.class_ "error" $ "Your requested page was not found." }
