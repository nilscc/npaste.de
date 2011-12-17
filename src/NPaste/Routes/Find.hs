module NPaste.Routes.Find
  ( viewR
  ) where

import Happstack.Server

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Parser


--------------------------------------------------------------------------------
-- Specific search queries

viewR :: NPaste ()
viewR = do
  CSS .= ["code/hk-pyg.css", "code.css", "view.css"]
  choice
    [ dir "t" tagR
    , do pastes    <- getRecentPastes Nothing 20 0 False -- TODO: support for user/limit/offset/hidden
         M_View    .= Nothing
         Title     .= Just "Recent pastes"
         HtmlBody  .= viewHtml Nothing pastes
    ]

tagR :: NPaste ()
tagR = choice
  [ methodM POST >> do
      decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
      tag <- body $ look "tag"
      rq <- askRq
      if validTag tag then do
        let url = "/v/t/" ++ tag
        PlainResponse rq .= seeOther url  . toResponse $ "Forwarding to npaste.de" ++ url
       else
        PlainResponse rq .= seeOther "/v" . toResponse $ "Invalid tag, forwarding to npaste.de/v"
  , path $ \t -> do
      unless (validTag t) mzero
      pastes <- findPastes 20 0 (S_Tag t)
      M_View   .= Just t
      Title    .= Just $ "Pastes for #" ++ t
      HtmlBody .= viewHtml (Just t) pastes
  ]
