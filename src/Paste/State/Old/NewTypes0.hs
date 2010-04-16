{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.NewTypes0 where

import Happstack.Data
import Happstack.State.ClockTime        (ClockTime (..))

import qualified Data.ByteString as BS

import Paste.State.Content
import Paste.State.ID
import Users.State.Old.User2            (User (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        newtype PUser           = PUser         { unUser            :: (Maybe User)         }
        newtype PId             = PId           { unPId             :: ID                   }
        newtype PDate           = PDate         { unPDate           :: ClockTime            }
        newtype PContent        = PContent      { unPContent        :: Content              }
        newtype PHash           = PHash         { unPHash           :: BS.ByteString        }
        newtype PFileType       = PFileType     { unPFileType       :: Maybe String         }
        newtype PDescription    = PDescription  { unPDescription    :: Maybe String         }
        newtype PHide           = PHide         { unPHide           :: Bool                 }
        newtype PTags           = PTags         { unPTags           :: [String]             }

    |])

-- newtype stuff
$(deriveSerialize ''PUser)
instance Version PUser

$(deriveSerialize ''PId)
instance Version PId

$(deriveSerialize ''PDate)
instance Version PDate

$(deriveSerialize ''PContent)
instance Version PContent

$(deriveSerialize ''PHash)
instance Version PHash

$(deriveSerialize ''PFileType)
instance Version PFileType

$(deriveSerialize ''PDescription)
instance Version PDescription

$(deriveSerialize ''PHide)
instance Version PHide

$(deriveSerialize ''PTags)
instance Version PTags
