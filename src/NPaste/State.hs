module NPaste.State
  ( addCookie

  , runNPaste, evalNPaste, execNPaste
  , runOutputM, evalOutputM, execOutputM

  , npasteNullState

  , choice
  , optional
  , (.=)
  ) where

import qualified Happstack.Server as HS
import NPaste.Html
import NPaste.Types


--------------------------------------------------------------------------------
-- * Happstack functions mapped to statefull NPaste monad

addCookie :: HS.CookieLife -> HS.Cookie -> NPaste ()
addCookie l c =
  modifyNP_ $ \s ->
    s{ runBeforeResponse = runBeforeResponse s ++ [HS.addCookie l c] }


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
  { responseFormat    = HtmlResponse
  , responseCode      = ResponseCode HS.ok
  , htmlContext       = nullContext
  , htmlFrame         = HtmlFrame mainFrame
  , htmlBody          = HtmlBody $ return ()
  , currentUser       = CurrentUser Nothing
  , runBeforeResponse = []
  }

-- ** Other

-- | Sensible version of `msum` that resets the state after a failed attempt
choice :: [NPaste a] -> NPaste a
choice val = do
  t <- get
  msum [ msum [ v, setNP t >> mzero ] | v <- val ]

optional :: NPaste a -> NPaste (Maybe a)
optional np = choice [ Just `fmap` np, return Nothing ]

-- | Convenient `setNP` alias
infixr 0 .=
(.=) :: ModifyNPasteState t
     => (a -> t)
     -> a
     -> NPaste ()
con .= val = setNP $ con val
