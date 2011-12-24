{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes,
             FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.State where

import Control.Concurrent.MState
import Happstack.Server

import NPaste.Types.Database
import NPaste.Types.Html

--------------------------------------------------------------------------------
-- * The server monads

type NPaste a  = MState NPasteState (ServerPartT IO) a

--------------------------------------------------------------------------------
-- * The server state

data NPasteState = NPasteState
  { responseFormat    :: ResponseFormat
  , responseCode      :: ResponseCode
  , htmlContext       :: HtmlContext
  , htmlFrame         :: HtmlFrame
  , htmlBody          :: HtmlBody
  , currentSession    :: CurrentSession
  , runBeforeResponse :: [ServerPart ()]
  }
  deriving Show

instance Show Html where
  show _ = "<html>"

instance Show (ServerPart a) where
  show _ = "<serverpart>"

data ResponseFormat
  = HtmlResponse
  | PartialHtmlResponse
  | PlainResponse Request (ServerPart Response)
  -- | JsonResponse -- TODO
  deriving Show

newtype ResponseCode   = ResponseCode   { unResponseCode   :: Response -> ServerPart Response } deriving Show
newtype HtmlBody       = HtmlBody       { unHtmlBody       :: Html }                            deriving Show
newtype HtmlFrame      = HtmlFrame      { unHtmlFrame      :: HtmlContext -> HtmlBody -> Html } deriving Show
newtype CurrentSession = CurrentSession { unCurrentSession :: Maybe Session                   } deriving Show

-- ** State modification

-- | Minimal complete definition: `modifyNP` and `getsNP`
class ModifyNPasteState t where
  modifyNP  :: (t -> (a,t)) -> NPaste a
  modifyNP_ :: (t -> t)     -> NPaste ()
  setNP     ::  t           -> NPaste ()
  getNP     :: NPaste t
  getsNP    :: (t -> a)     -> NPaste a
  modifyNP_ f = modifyNP  $ \t -> ((),f t)
  setNP     t = modifyNP_ $ \_ -> t
  getNP       = getsNP id

instance ModifyNPasteState NPasteState where
  modifyNP f = modifyM $ \st -> f st
  getsNP f   = gets f

instance ModifyNPasteState ResponseFormat where
  modifyNP f = modifyM $ \st ->
                 let (a,rsp) = f $ responseFormat st
                  in (a,st{ responseFormat = rsp })
  getsNP f   = gets $ f . responseFormat

instance ModifyNPasteState ResponseCode where
  modifyNP f = modifyM $ \st ->
                 let (a,c) = f $ responseCode st
                  in (a,st{ responseCode = c })
  getsNP f   = gets $ f . responseCode

instance ModifyNPasteState HtmlContext where
  modifyNP f = modifyM $ \st ->
                 let (a,frm) = f $ htmlContext st
                  in (a,st{ htmlContext = frm })
  getsNP f   = gets $ f . htmlContext

instance ModifyNPasteState HtmlFrame where
  modifyNP f = modifyM $ \st ->
                 let (a,frm) = f $ htmlFrame st
                  in (a,st{ htmlFrame = frm })
  getsNP f   = gets $ f . htmlFrame

instance ModifyNPasteState HtmlBody where
  modifyNP f = modifyM $ \st ->
                 let (a,bdy) = f $ htmlBody st
                  in (a,st{ htmlBody = bdy })
  getsNP f   = gets $ f . htmlBody

instance ModifyNPasteState CurrentSession where
  modifyNP f = modifyM $ \st ->
                 let (a,s) = f $ currentSession st
                  in (a,st{ currentSession = s })
  getsNP f   = gets $ f . currentSession

instance ModifyNPasteState Title where
  modifyNP f = modifyNP $ \ctxt ->
                 let (a,t) = f $ title ctxt
                  in (a,ctxt{ title = t })
  getsNP f   = gets $ f . title . htmlContext

instance ModifyNPasteState Menu where
  modifyNP f = modifyNP $ \ctxt ->
                 let (a,m) = f $ menu ctxt
                  in (a,ctxt{ menu = m })
  getsNP f   = gets $ f . menu . htmlContext

instance ModifyNPasteState ActiveMenu where
  modifyNP f = modifyNP $ \m ->
                 let (a,s) = f $ activeMenuSection m
                  in (a,m{ activeMenuSection = s })
  getsNP f   = gets $ f . activeMenuSection . menu . htmlContext

instance ModifyNPasteState MenuStructure where
  modifyNP f = modifyNP $ \m ->
                 let (a,s) = f $ menuStructure m
                  in (a,m{ menuStructure = s })
  getsNP f   = gets $ f . menuStructure . menu . htmlContext

instance ModifyNPasteState CSS where
  modifyNP f = modifyNP $ \ctxt ->
                 let (a,c) = f $ css ctxt
                  in (a,ctxt{ css = c })
  getsNP f   = gets $ f . css . htmlContext

instance ModifyNPasteState Script where
  modifyNP f = modifyNP $ \info ->
                 let (a,c) = f $ script info
                  in (a,info{ script = c })
  getsNP f   = gets $ f . script . htmlContext
