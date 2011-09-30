{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read where

import Happstack.Server
-- import Control.Monad.Trans

import NPaste.Database
import NPaste.Html
import NPaste.Html.Read
import NPaste.Types


readR :: ServerPart Response
readR = msum
  [ path $ \pId -> do
      let mu = Nothing -- TODO
      pinfo <- getPostById mu pId
      pcont <- getContent  mu pId
      return . toResponse . mainFrame $ nullBody
        { css    = ["code/hk-pyg.css", "code.css"]
        , html   = readHtml pinfo pcont
        }
  , do
      posts    <- getRecentPosts Nothing 20 0 False
      contents <- forM posts $ \PostInfo{ p_id, p_user_id } -> do
                    mu <- getUserById p_user_id
                    getContent mu p_id
      return . toResponse . mainFrame $ nullBody
        { css    = ["code/hk-pyg.css", "code.css"]
        , html   = recentHtml $ zip posts contents
        }
  ]
