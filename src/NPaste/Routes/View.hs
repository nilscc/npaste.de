{-# LANGUAGE ViewPatterns #-}

module NPaste.Routes.View where

import Happstack.Server

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Parser
import NPaste.Utils


--------------------------------------------------------------------------------
-- Specific search queries

viewR :: NPaste ()
viewR = do

  CSS .= ["code.css", "view.css"]

  f <- msum
    [ do methodM POST
         decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
         body $ look "filter"
    , fmap unwords buildFilterFromUrl
    ]

  case parseFilter f of
       Left err -> do
         ActiveMenu .= Just $ M_View Nothing
         Title      .= Just "View recent pastes"
         HtmlBody   .= viewHtml (Just f) (Left err)
       Right fl@(filterToSearch -> Just s) -> do
         p <- runQuery . findPastes 20 0 =<< addHiddenFilter s
         ActiveMenu .= Just $ M_View (Just fl)
         Title      .= Just "View pastes (filtered)"
         HtmlBody   .= viewHtml (Just f) (Right p)
       _ -> do
         p <- runQuery $ getRecentPastes Nothing 20 0 False -- TODO: support for user/limit/offset/hidden
         ActiveMenu .= Just $ M_View Nothing
         Title      .= Just "View recent pastes"
         HtmlBody   .= viewHtml Nothing (Right p)

buildFilterFromUrl :: NPaste [String]
buildFilterFromUrl = msum
  [ dir "id"   $ path $ \i -> fmap (("/" ++ i ++ "/")  :) buildFilterFromUrl
  , dir "tag"  $ path $ \t -> fmap (("#" ++ t)         :) buildFilterFromUrl
  -- , dir "user" $ path $ \u -> fmap (("@" ++ u)         :) buildFilterFromUrl
  , dir "lang" $ path $ \l -> fmap ((l)                :) buildFilterFromUrl
  , dir "desc" $ path $ \d -> fmap (("\"" ++ d ++ "\""):) buildFilterFromUrl
  , return []
  ]

-- Show hidden pastes if a hidden paste is part of the query, otherwise hide
-- them
addHiddenFilter :: Search -> NPaste Search
addHiddenFilter s = do
  h  <- containsHiddenId s
  if h then
     return s
   else do
     mu <- getCurrentUser
     return $ S_And `flip` s $
       maybe (S_PasteHidden False) (S_Or (S_PasteHidden False) . S_User) mu
 where
  containsHiddenId :: Search -> NPaste Bool
  containsHiddenId (S_And s1 s2) =
    (||) <$> containsHiddenId s1
         <*> containsHiddenId s2
  containsHiddenId (S_Or s1 s2) =
    (||) <$> containsHiddenId s1
         <*> containsHiddenId s2
  containsHiddenId (S_PasteId i) = do
    mp <- runQuery $ getPasteById i
    return $ maybe False pasteHidden mp
  containsHiddenId _ = return False
