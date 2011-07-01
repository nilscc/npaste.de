module NPaste.Routes.Index where

import Happstack.Server

import NPaste.Html
import NPaste.Types


indexR :: ServerPart Html
indexR = msum
  [ do methodM POST
       pdata <- getIndexPostData
       return $ mainFrame $ nullBody
         { css = ["index.css"]
         , html = indexHtml pdata
         }
  , do methodM GET
       return $ mainFrame $ nullBody
         { css = ["index.css"]
         , html = indexHtml nullPostData
         }
  ]


--------------------------------------------------------------------------------
-- Post data

getIndexPostData :: ServerPart IndexPostData
getIndexPostData = do
  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
  fmap IndexPostData $ body lookPairs

nullPostData :: IndexPostData
nullPostData = IndexPostData []
