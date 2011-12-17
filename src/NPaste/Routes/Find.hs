module NPaste.Routes.Find
  ( viewR
  ) where

import Happstack.Server

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Utils


--------------------------------------------------------------------------------
-- Specific search queries

viewR :: NPaste ()
viewR = do
  setNP M_Recent -- menu location
  CSS .= ["code/hk-pyg.css", "code.css", "view.css"]
  choice
    [ dir "t" tagR
    , do pastes    <- getRecentPastes Nothing 20 0 False -- TODO: support for user/limit/offset/hidden
         Title     .= Just "Recent pastes"
         HtmlBody  .= viewHtml Nothing pastes
    ]

tagR :: NPaste ()
tagR = choice
  [ methodM POST >> do
      decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
      tag <- body $ look "tag"
      if validTag tag then do
        let url = "/v/t/" ++ tag
        rq <- askRq
        PlainResponse rq .= seeOther url . toResponse $
          "Go to npaste.de" ++ url ++ " to see the results of your query"
       else
        mzero
  , path $ \t -> do
      pastes <- findPastes 20 0 (S_Tag t)
      Title    .= Just $ "Pastes for #" ++ t
      HtmlBody .= viewHtml (Just t) pastes
  ]
