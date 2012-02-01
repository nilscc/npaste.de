module NPaste.Utils.Happstack where

import Data.ByteString      as B (concat)
import Data.ByteString.Lazy      (ByteString, toChunks)
import Control.Arrow
import Happstack.Server
import NPaste.Utils.State
import NPaste.Types


--------------------------------------------------------------------------------
-- Happstack helper functions

getPostData :: NPaste PostData
getPostData = choice
  [ do method POST
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap toPostData $ body lookPairsBS
  , return nullPostData
  ]

getPostData' :: NPaste PostData
getPostData' = choice
  [ do methodM POST
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap toPostData $ body lookPairsBS
  , return nullPostData
  ]

nullPostData :: PostData
nullPostData = PostData []

toPostData :: [(String, Either FilePath ByteString)] -> PostData
toPostData = PostData . map (second (right (B.concat . toChunks)))
