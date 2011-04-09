{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.PasteEntry ( PasteEntry (..) ) where

import Happstack.Data

import Paste.State.NewTypes
import qualified Paste.State.Old.PasteEntry7 as Old

deriveAll [''Show, ''Eq, ''Ord] [d|

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

  |]

deriveSerialize ''PasteEntry

instance Version PasteEntry where
    mode = extension 8 (Proxy :: Proxy Old.PasteEntry)

instance Migrate Old.PasteEntry PasteEntry where
    migrate (Old.PasteEntry puser pid pdate pcontent phash pfiletype pdesc phide ptags) =
        PasteEntry (migrate puser)
                   pid
                   pdate
                   pcontent
                   phash
                   pfiletype
                   pdesc
                   phide
                   ptags
