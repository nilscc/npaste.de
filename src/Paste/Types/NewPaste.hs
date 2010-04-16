module Paste.Types.NewPaste
    ( PostError (..)
    ) where

import Control.Monad.Error.Class    (Error (..))
import System.Time                  (TimeDiff (..), timeDiffToString)

import Paste.State
-- import Users.State.User             (User)

type MaxSize = Int

-- | Define post error data
data PostError = MD5Exists PasteEntry   -- ^ md5 of a paste entry already exists
               | MaxPastes TimeDiff     -- ^ max number of pastes reached, postable in ** minutes again
               | ContentTooBig MaxSize  -- ^ max size in kb
               | DescriptionTooBig Int  -- ^ description is limited too, number of chars
               -- | NoUser                 -- ^ no user given
               | NoPassword             -- ^ no password given
               | NoContent              -- ^ no content given
               | WrongLogin             -- ^ wrong login name
               | InvalidID              -- ^ invalid ID
               | IsSpam
               -- | NoError IDType         -- ^ URL of paste
               | OtherPostError String  -- ^ other

-- | Show instance
instance Show PostError where
    -- show (NoError id)       = "Paste successful."
    -- show (MD5Exists pe)     = "Paste already exists at /" ++ (unId . unPId . pId $ pe) ++ "/"
    show (MD5Exists pe)     = "http://npaste.de/" ++ (unId . unPId . pId $ pe) ++ "/" -- just return the url
    show (MaxPastes tdiff)  = "Max number of pastes reached. Please try again in " ++ timeDiffToString tdiff ++ "."
    show NoContent          = "No content given."
    show NoPassword         = "No password given."
    show (ContentTooBig ms) = "Content size too big (max " ++ show ms ++ "kb)."
    show (DescriptionTooBig n) = "Description too big (max " ++ show n ++ " chars)."
    show WrongLogin         = "Wrong username/password."
    show InvalidID          = "Invalid ID."
    show IsSpam             = "Something went wrong." -- we don't want to let everybody know :)
    show (OtherPostError s) = s

instance Error PostError where
    strMsg = OtherPostError
