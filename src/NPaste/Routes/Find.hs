{-# LANGUAGE ViewPatterns #-}

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

  f <- choice
    [ do methodM POST
         decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
         body $ look "filter"
    , fmap unwords buildFilterFromUrl
    ]

  CSS       .= ["code/hk-pyg.css", "code.css", "view.css"]

  case parseFilter f of
       Right fl@(filterToSearch -> Just s) -> do
         p <- findPastes 20 0 s
         M_View    .= Just fl
         Title     .= Just "Recent pastes (filtered)"
         HtmlBody  .= viewHtml (Just f) (Right p)
       Left err -> do
         M_View    .= Nothing
         Title     .= Just "Recent pastes"
         HtmlBody  .= viewHtml (Just f) (Left err)
       _ -> do
         p <- getRecentPastes Nothing 20 0 False -- TODO: support for user/limit/offset/hidden
         M_View    .= Nothing
         Title     .= Just "Recent pastes"
         HtmlBody  .= viewHtml Nothing (Right p)

buildFilterFromUrl :: NPaste [String]
buildFilterFromUrl = choice
  [ dir "id"   $ path $ \i -> fmap (("/" ++ i ++ "/")  :) buildFilterFromUrl
  , dir "tag"  $ path $ \t -> fmap (("#" ++ t)         :) buildFilterFromUrl
  , dir "user" $ path $ \u -> fmap (("@" ++ u)         :) buildFilterFromUrl
  , dir "lang" $ path $ \l -> fmap ((l)                :) buildFilterFromUrl
  , dir "desc" $ path $ \d -> fmap (("\"" ++ d ++ "\""):) buildFilterFromUrl
  , return []
  ]
  
