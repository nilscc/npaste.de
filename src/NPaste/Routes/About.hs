module NPaste.Routes.About
  ( aboutR
  ) where

import Happstack.Server

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html

aboutR :: NPaste ()
aboutR = do
  ActiveMenu .= M_About
  CSS        .= ["document.css"]
  choice
    [ nullDir >> do
      Title    .= Just "About"
      HtmlBody .= aboutHtml
    , dir "howto" $ do
      Title    .= Just "How to"
      HtmlBody .= howtoHtml
    , dir "contact" $ do
      Title    .= Just "Contact"
      HtmlBody .= contactHtml
    , dir "statistics" $ do
      u        <- getNumberOfUsers
      p        <- getNumberOfPastes
      Title    .= Just "Statistics"
      HtmlBody .= statisticsHtml u p
    -- , dir "disclaimer" $ do
    --   Title    .= Just "Disclaimer"
    --   HtmlBody .= disclaimerHtml
    ]
