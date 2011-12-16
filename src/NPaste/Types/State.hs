{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes,
             FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.State where

import Control.Monad.Error
import Control.Concurrent.MState
import Happstack.Server

import NPaste.Types.Database
import NPaste.Types.Error
import NPaste.Types.Html

--------------------------------------------------------------------------------
-- * The server monads

type NPaste a = ErrorT NPasteError (MState NPasteState (ServerPartT IO)) a
type OutputM a = MState NPasteState (ServerPartT IO) a


--------------------------------------------------------------------------------
-- * The server state

data NPasteState = NPasteState
  { responseFormat  :: ResponseFormat
  , responseCode    :: ResponseCode
  , htmlContext     :: HtmlContext
  , htmlFrame       :: HtmlFrame
  , htmlBody        :: HtmlBody
  , currentUser     :: Maybe User
  }
  deriving Show

instance Show Html where
  show _ = "<html>"

data ResponseFormat
  = HtmlResponse
  | PartialHtmlResponse
  | PlainResponse Response
  -- | JsonResponse -- TODO
  deriving Show

newtype ResponseCode = ResponseCode { unResponseCode :: Response -> ServerPart Response } deriving Show
newtype HtmlBody     = HtmlBody     { unHtmlBody     :: Html }                            deriving Show
newtype HtmlFrame    = HtmlFrame    { unHtmlFrame    :: HtmlContext -> HtmlBody -> Html } deriving Show

-- ** State modification

-- | Minimal complete definition: `modifyNP`
class ModifyNPasteState t where
  modifyNP  :: (t -> (a,t)) -> NPaste a
  setNP     ::  t           -> NPaste ()
  modifyNP_ :: (t -> t)     -> NPaste ()
  modifyNP_ f = modifyNP  $ \t -> ((),f t)
  setNP     t = modifyNP_ $ \_ -> t

instance ModifyNPasteState NPasteState where
  modifyNP f = lift . modifyM $ \st -> f st

instance ModifyNPasteState ResponseFormat where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,rsp) = f $ responseFormat st
                  in (a,st{ responseFormat = rsp })

instance ModifyNPasteState ResponseCode where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,c) = f $ responseCode st
                  in (a,st{ responseCode = c })

instance ModifyNPasteState HtmlContext where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,frm) = f $ htmlContext st
                  in (a,st{ htmlContext = frm })

instance ModifyNPasteState HtmlFrame where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,frm) = f $ htmlFrame st
                  in (a,st{ htmlFrame = frm })

instance ModifyNPasteState HtmlBody where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,bdy) = f $ htmlBody st
                  in (a,st{ htmlBody = bdy })

instance ModifyNPasteState (Maybe User) where
  modifyNP f = lift . modifyM $ \st ->
                 let (a,usr) = f $ currentUser st
                  in (a,st{ currentUser = usr })

instance ModifyNPasteState Title where
  modifyNP f = modifyNP $ \ctxt ->
                 let (a,t) = f $ title ctxt
                  in (a,ctxt{ title = t })

instance ModifyNPasteState MenuSection where
  modifyNP f = modifyNP $ \ctxt ->
                 let (a,c) = f $ section ctxt
                  in (a,ctxt{ section = c })

instance ModifyNPasteState CSS where
  modifyNP f = modifyNP $ \info ->
                 let (a,c) = f $ css info
                  in (a,info{ css = c })

instance ModifyNPasteState Script where
  modifyNP f = modifyNP $ \info ->
                 let (a,c) = f $ script info
                  in (a,info{ script = c })
