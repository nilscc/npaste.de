module App.Helper
    ( isDir
    ) where

import Control.Monad ( MonadPlus(..) )
import Happstack.Server
    ( guardRq
    , rqUri
    , ServerMonad (..)
    )

isDir :: (ServerMonad m, MonadPlus m) => m ()
isDir = guardRq $ \rq -> last (rqUri rq) == '/'
