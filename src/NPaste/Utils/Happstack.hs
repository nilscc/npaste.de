module NPaste.Utils.Happstack where

import Happstack.Server
import NPaste.Utils.State
import NPaste.Types


--------------------------------------------------------------------------------
-- Happstack helper functions

getPostData :: NPaste PostData
getPostData = choice
  [ do method POST
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap PostData $ body lookPairs
  , return nullPostData
  ]

getPostData' :: NPaste PostData
getPostData' = choice
  [ do methodM POST
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap PostData $ body lookPairs
  , return nullPostData
  ]

nullPostData :: PostData
nullPostData = PostData []
