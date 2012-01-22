module NPaste.State
  ( addCookie
  , expireCookie

  , runNPaste, evalNPaste, execNPaste

  , npasteNullState

    -- * Menu stuff
  , setupUserMenu

    -- * User management
  , getCurrentUser
  , requireUser
  , requireNoUser

    -- * Session management
  , getCurrentSession
  , requireSession
  , requireNoSession
  ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import qualified Happstack.Server      as HS

import NPaste.Database
import NPaste.Html
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- * Happstack functions mapped to statefull NPaste monad

addCookie :: HS.CookieLife -> HS.Cookie -> NPaste ()
addCookie l c =
  modifyNP_ $ \s ->
    s{ runBeforeResponse = runBeforeResponse s ++ [HS.addCookie l c] }

expireCookie :: String -> NPaste ()
expireCookie n =
  modifyNP_ $ \s ->
    s{ runBeforeResponse = runBeforeResponse s ++ [HS.expireCookie n] }


--------------------------------------------------------------------------------
-- * State management

runNPaste :: NPasteState -> NPaste a -> ServerPart (a, NPasteState)
runNPaste s n = runMState n s

evalNPaste :: NPasteState -> NPaste a -> ServerPart a
evalNPaste s n = fmap fst $ runNPaste s n

execNPaste :: NPasteState -> NPaste a -> ServerPart NPasteState
execNPaste s n = fmap snd $ runNPaste s n

-- ** Default values

npasteNullState :: NPasteState
npasteNullState = NPasteState
  { responseFormat    = HtmlResponse
  , responseCode      = ResponseCode HS.ok
  , htmlContext       = nullContext
  , htmlFrame         = HtmlFrame mainFrame
  , htmlBody          = HtmlBody $ return ()
  , currentSession    = CurrentSession Nothing
  , runBeforeResponse = []
  }


--------------------------------------------------------------------------------
-- * User management

-- | Get the current session/user from the session cookie. This function should
-- probably be used only once (hence the awfully long name).
initSession :: NPaste ()
initSession = try $ do
  s  <- HS.lookCookieValue "sessionId"
  rq <- HS.askRq
  let ip = fromMaybe (fst $ HS.rqPeer rq) $
            getHeader' "x-forwarded-for" (HS.rqHeaders rq)
      ua = fromMaybe "" $
            getHeader' "user-agent"      (HS.rqHeaders rq)
  ms <- getSession s ip ua
  CurrentSession .= ms
 where
  try f = choice [f, return ()]
  getHeader' h = fmap B8.unpack . HS.getHeader h

getCurrentUser :: NPaste (Maybe User)
getCurrentUser = (join . fmap sessionUser) `fmap` getCurrentSession

requireUser :: NPaste User
requireUser =
  maybe mzero return =<< getCurrentUser

requireNoUser :: NPaste ()
requireNoUser =
  maybe (return ()) (const mzero) =<< getCurrentUser


--------------------------------------------------------------------------------
-- * Session management

getCurrentSession :: NPaste (Maybe Session)
getCurrentSession = unCurrentSession `fmap` getNP

requireSession :: NPaste Session
requireSession =
  maybe mzero return =<< getCurrentSession

requireNoSession :: NPaste ()
requireNoSession =
  maybe (return ()) (const mzero) =<< getCurrentSession


--------------------------------------------------------------------------------
-- * Menu generation

setupUserMenu :: NPaste ()
setupUserMenu = do
  initSession
  mu <- getCurrentUser
  case mu of
       Just u  -> MenuStructure .= userMenu u
       Nothing -> MenuStructure .= anonMenu
