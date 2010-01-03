{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.PasteEntry ( PasteEntry (..) ) where

import Happstack.Data

import Paste.State.NewTypes
import qualified Paste.State.Old.PasteEntry6 as Old

$(deriveAll [''Show, ''Eq, ''Default]
    [d|

        -- | PasteEntry: Simple paste entry
        data PasteEntry = PasteEntry
            { user          :: PUser
            , pId           :: PId
            , date          :: PDate
            , content       :: PContent
            , md5hash       :: PHash
            , filetype      :: PFileType
            , description   :: PDescription
            , hide          :: PHide
            , tags          :: PTags
            }

    |])

instance Ord PasteEntry where
    c1 `compare` c2 = (unPDate $ date c1) `compare` (unPDate $ date c2)

$(deriveSerialize ''PasteEntry)
instance Version PasteEntry where
    mode = extension 7 (Proxy :: Proxy Old.PasteEntry)

instance Migrate Old.PasteEntry PasteEntry where
    migrate (Old.PasteEntry u i d c md f _ desc hide tags responses) =
        PasteEntry (PUser u)
                   (PId i)
                   (PDate d)
                   (PContent c)
                   (PHash md)
                   (PFileType f)
                   (PDescription desc)
                   (PHide hide)
                   (PTags tags)
