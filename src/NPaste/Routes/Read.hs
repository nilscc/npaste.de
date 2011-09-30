module NPaste.Routes.Read where

import Happstack.Server
-- import Control.Monad.Trans

import NPaste.Database
import NPaste.Html
import NPaste.Types

import NPaste.Html.Read

readR :: ServerPart Response
readR = path $ \pId -> do
  let mu = Nothing -- TODO
  pinfo <- getPostById mu pId
  pcont <- getContent  mu pId
  return . toResponse . mainFrame $ nullBody
    { css    = ["code/hk-pyg.css", "code.css"]
    , html   = readHtml pinfo pcont
    }
