module Paste.Types
    ( PasteResponse (..)
    , PostData (..)
    , ShowOnIndex (..)
    ) where

import Paste.State

type MaxSize = Int
-- | Define post error data
data PasteResponse = NoError IDType         -- ^ URL of paste
                   | EmptyContent           -- ^ no content given
                   | ContentTooBig MaxSize  -- ^ max size in kb
                   | WrongUserLogin         -- ^ wrong login name
                   | WrongUserPassword      -- ^ wrong password
                   | InvalidID              -- ^ invalid ID
                   | Other                  -- ^ other

-- | Show instance
instance Show PasteResponse where
    show (NoError id)       = "Paste successful."
    show EmptyContent       = "No content given."
    show (ContentTooBig ms) = "Content size too big (max " ++ show ms ++ "kb)."
    show WrongUserLogin     = "Wrong login name."
    show WrongUserPassword  = "Wrong password."
    show InvalidID          = "Invalid ID."
    show Other              = "Something went wrong."


-- | Define post data
data PostData = PostData { cont     :: String       -- ^ Necessary content
                         , un       :: Maybe String -- ^ Username
                         , pwd      :: Maybe String -- ^ Password
                         , ft       :: Maybe String -- ^ Filetype
                         , sub      :: Bool         -- ^ Submit button from form
                         , idType   :: IDType       -- ^ handle custom IDs request
                         , idReq    :: Maybe String -- ^ string of the requested ID
                         }


-- | Data definition for index page rendering
data ShowOnIndex = ShowOnIndex { postData       :: Maybe PostData
                               , pasteResponse  :: Maybe PasteResponse
                               }


