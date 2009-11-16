{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.PasteEntry ( PasteEntry (..) ) where

import Happstack.Data
import Happstack.State.ClockTime (ClockTime (..))

import Users.State          (User (..))
import Paste.State.ID       (ID (..))
import Paste.State.Content  (Content (..))

-- import qualified Data.ByteString as BS


--------------------------------------------------------------------------------
-- Migration import
-- import qualified Paste.State.PasteEntry (PasteEntry (..)) as Old
-- 
--------------------------------------------------------------------------------

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | PasteEntry: Simple paste entry
        data PasteEntry = PasteEntry
                    { user      :: Maybe User
                    , pId       :: ID
                    , date      :: ClockTime
                    , content   :: Content
                    -- , md5hash   :: BS.ByteString
                    , filetype  :: Maybe String
                    }
    |])

$(deriveSerialize ''PasteEntry)
instance Version PasteEntry where
    mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.PasteEntry)

{-
instance Migration Old.PasteEntry PasteEntry where
    migrate (Old.PasteEntry u i d c f) = PasteEntry u i d c BS.empty f
-}
