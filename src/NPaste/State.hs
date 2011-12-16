module NPaste.State
  ( runNPaste, evalNPaste, execNPaste
  , runOutputM, evalOutputM, execOutputM

  , npasteNullState
  , resetNPasteState
  , reset
  ) where

import Happstack.Server
import NPaste.Html
import NPaste.Types


--------------------------------------------------------------------------------
-- * State management

runNPaste :: NPasteState -> NPaste a -> ServerPart (Either NPasteError a, NPasteState)
runNPaste s n = runMState (runErrorT n) s

evalNPaste :: NPasteState -> NPaste a -> ServerPart (Either NPasteError a)
evalNPaste s n = fmap fst $ runNPaste s n

execNPaste :: NPasteState -> NPaste a -> ServerPart NPasteState
execNPaste s n = fmap snd $ runNPaste s n

-- ** Output stuff

runOutputM :: NPasteState -> OutputM a -> ServerPart (a, NPasteState)
runOutputM s n = runMState n s

evalOutputM :: NPasteState -> OutputM a -> ServerPart a
evalOutputM s n = fmap fst $ runMState n s

execOutputM :: NPasteState -> OutputM a -> ServerPart NPasteState
execOutputM s n = fmap snd $ runMState n s

-- ** Default values

npasteNullState :: NPasteState
npasteNullState = NPasteState
  { responseFormat  = HtmlResponse
  , responseCode    = ResponseCode ok
  , htmlContext     = nullContext
  , htmlFrame       = HtmlFrame mainFrame
  , htmlBody        = HtmlBody $ return ()
  , currentUser     = Nothing
  }

resetNPasteState :: NPaste ()
resetNPasteState = setNP npasteNullState

reset :: NPaste a
reset = resetNPasteState >> mzero
