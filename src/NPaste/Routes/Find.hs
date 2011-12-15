module NPaste.Routes.Find
  ( findR
  , tagR
  ) where

import Happstack.Server

import NPaste.Database
import NPaste.Types
import NPaste.Html

--------------------------------------------------------------------------------
-- Finding

findR :: ServerPart Response
findR = do
  sres <- getSearch
  case sres of
       Just s -> do
         pastes <- findPastes 20 0 s
         return . toResponse . mainFrame $ nullBody
           { css  = ["code/hk-pyg.css", "code.css", "recent.css"]
           , html = findHtml "Your search results:" pastes
           }
       Nothing -> do
         return . toResponse . mainFrame $ nullBody
           { css  = ["recent.css"]
           , html = findHtmlNothingFound
           }

getSearch :: ServerPart (Maybe Search)
getSearch = msum
  [ path $ \p -> case p of
      _ | p `elem` ["t","tag"]             -> keepSearching S_Tag
        | p `elem` ["l","lang","language"] -> keepSearching S_PasteType
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
        -> (Maybe Search -> ServerPart (Maybe Search))
        -> ServerPart (Maybe Search)
search' constr doit = msum
  [ path $ doit . Just . constr
  , return Nothing
  ]


--------------------------------------------------------------------------------
-- Specific search queries

tagR :: ServerPart Response
tagR = path $ \t -> do
  pastes <- findPastes 20 0 (S_Tag t)
  return . toResponse . mainFrame $ nullBody
    { css  = ["code/hk-pyg.css", "code.css", "recent.css"]
    , html = tagHtml t pastes
    }
