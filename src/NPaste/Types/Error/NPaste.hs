module NPaste.Types.Error.NPaste where

import Control.Monad.Error

data NPasteError = NPasteError

instance Error NPasteError where
  noMsg = NPasteError
