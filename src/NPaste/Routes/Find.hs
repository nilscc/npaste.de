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

--------------------------------------------------------------------------------
-- Finding

findR :: NPaste ()
findR = do
  setNP M_Other -- menu location
  sres <- getSearch
  case sres of
       Just s -> do
         pastes <- findPastes 20 0 s
         CSS      .= ["code/hk-pyg.css", "code.css", "recent.css"]
         HtmlBody .= findHtml "Your search results:" pastes
       Nothing -> do
         CSS       .= ["recent.css"]
         HtmlBody  .= findHtmlNothingFound

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
tagR = path $ \t -> do
  pastes <- findPastes 20 0 (S_Tag t)
  CSS      .= ["code/hk-pyg.css", "code.css", "recent.css"]
  HtmlBody .= tagHtml t pastes
