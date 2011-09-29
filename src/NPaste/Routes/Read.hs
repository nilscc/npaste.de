module NPaste.Routes.Read where

import Happstack.Server
import Control.Monad.Trans

import NPaste.Database

readR :: ServerPart Response
readR = path $ \pId -> do
  let mu = Nothing -- TODO
  pinfo <- getPostById mu pId
  pcont <- getContent  mu pId
  liftIO $ print pinfo
  return . toResponse $ show pcont
