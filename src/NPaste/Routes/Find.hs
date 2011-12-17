module NPaste.Routes.Find
  ( findR
  , tagR
  ) where

-- import Data.ByteString.Char8 (pack)
import Happstack.Server

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Utils

--------------------------------------------------------------------------------
-- Finding

findR :: NPaste ()
findR = do
  sres <- getSearch
  case sres of
       Just s -> do
         pastes <- findPastes 20 0 s
         CSS      .= ["code/hk-pyg.css", "code.css", "recent.css"]
         HtmlBody .= findHtml "Your search results:" pastes
       Nothing -> do
         CSS      .= ["recent.css"]
         HtmlBody .= findHtmlNothingFound

getSearch :: NPaste (Maybe Search)
getSearch = choice
  [ path $ \p -> case p of
      _ | p `elem` ["t","tag"]             -> keepSearching S_Tag
        | p `elem` ["l","lang","language"] -> keepSearching S_PasteType
        -- | p `elem` ["c","cont","content"]  -> keepSearching (S_PasteCont . pack)
        | p `elem` ["u","usr","user"]      -> keepSearching S_UserName
        | otherwise                        -> mzero
  , return Nothing
  ]
 where
  keepSearching constr = search' constr $ \sres ->
    case sres of
         Just s -> do
           rest <- getSearch
           return $ Just (maybe s (S_And s) rest)
         Nothing ->
           return Nothing

search' :: (String -> Search)
        -> (Maybe Search -> NPaste (Maybe Search))
        -> NPaste (Maybe Search)
search' constr doit = choice
  [ path $ doit . Just . constr
  , return Nothing
  ]


--------------------------------------------------------------------------------
-- Specific search queries

tagR :: NPaste ()
tagR =  do
  setNP M_Tags
  choice
    [ methodM POST >> do
        decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
        tag <- body $ look "tag"
        if validTag tag then do
          let url = "/t/" ++ tag
          rq <- askRq
          PlainResponse rq .= seeOther url . toResponse $ "Go to npaste.de" ++ url ++ " to see the results of your query"
         else
          mzero
    , path $ \t -> do
        pastes <- findPastes 20 0 (S_Tag t)
        Title    .= Just $ "Pastes for #" ++ t
        CSS      .= ["code/hk-pyg.css", "code.css", "recent.css"]
        Script   .= ["recent.js"]
        HtmlBody .= tagHtml t pastes
    , do
        Title    .= Just "Searching for tags"
        CSS      .= ["code/hk-pyg.css", "code.css", "recent.css"]
        Script   .= ["recent.js"]
        HtmlBody .= emptyTagHtml
    ]
