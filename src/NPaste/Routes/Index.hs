module NPaste.Routes.Index where

import Happstack.Server

import NPaste.Html
import NPaste.Types


indexR :: ServerPart Response
indexR = do
  pdata <- msum
    [ methodM POST >> getIndexPostData
    , return nullPostData ]
  return . toResponse . mainFrame $ nullBody
    { css    = ["index.css"]
    -- , script = ["index.js"]
    , html = indexHtml pdata
    }


--------------------------------------------------------------------------------
-- Post data

getIndexPostData :: ServerPart IndexPostData
getIndexPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 4096 4096 4096)
  fmap IndexPostData $ body lookPairs

nullPostData :: IndexPostData
nullPostData = IndexPostData []
