module NPaste.Routes.Read where

import Happstack.Server
import Control.Monad.Trans

import NPaste.Database
import NPaste.Html
import NPaste.Types

import NPaste.Html.Read

readR :: ServerPart Response
readR = path $ \pId -> do
  let mu = Nothing -- TODO
  pinfo <- getPostById mu pId
  pcont <- getContent  mu pId
  liftIO $ print pinfo

  return . toResponse . mainFrame $ nullBody
    { css    = ["compact.css"]
    , html   = readHtml pinfo pcont
    }
