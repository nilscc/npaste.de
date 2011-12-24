module NPaste.Utils.Happstack where

import Happstack.Server
import NPaste.Utils.State
import NPaste.Types


--------------------------------------------------------------------------------
-- Happstack helper functions

getPostData :: NPaste PostData
getPostData = choice
  [ do methodM POST
       liftIO $ putStrLn $ "getPostData called: lookPairs" -- debug
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap PostData $ body lookPairs
  , do liftIO $ putStrLn $ "getPostData called: nullPostData" -- debug
       return nullPostData
  ]

getPostData' :: NPaste PostData
getPostData' = choice
  [ do method POST
       liftIO $ putStrLn $ "getPostData called: lookPairs" -- debug
       _ <- optional $ decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
       fmap PostData $ body lookPairs
  , do liftIO $ putStrLn $ "getPostData called: nullPostData" -- debug
       return nullPostData
  ]

nullPostData :: PostData
nullPostData = PostData []
