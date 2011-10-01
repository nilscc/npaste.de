module NPaste.Types.NPaste
  ( NPaste
  , runNPaste, evalNPaste, execNPaste
  , OutputM
  , runOutputM, evalOutputM, execOutputM
  ) where

import Control.Concurrent.MState
import Control.Monad.Error
import Happstack.Server
import Happstack.Server.Internal.MonadPeelIO ()

import NPaste.Types.Error
import NPaste.Types.State

type NPaste a = ErrorT NPasteError (MState NPasteState (ServerPartT IO)) a

runNPaste :: NPasteState -> NPaste a -> ServerPart (Either NPasteError a, NPasteState)
runNPaste s n = runMState (runErrorT n) s

evalNPaste :: NPasteState -> NPaste a -> ServerPart (Either NPasteError a)
evalNPaste s n = fmap fst $ runNPaste s n

execNPaste :: NPasteState -> NPaste a -> ServerPart NPasteState
execNPaste s n = fmap snd $ runNPaste s n

type OutputM a = MState NPasteState (ServerPartT IO) a

runOutputM :: NPasteState -> OutputM a -> ServerPart (a, NPasteState)
runOutputM s n = runMState n s

evalOutputM :: NPasteState -> OutputM a -> ServerPart a
evalOutputM s n = fmap fst $ runMState n s

execOutputM :: NPasteState -> OutputM a -> ServerPart NPasteState
execOutputM s n = fmap snd $ runMState n s
