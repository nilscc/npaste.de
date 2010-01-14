{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Old.Content1 ( Content (..) ) where

import Happstack.Data

import Codec.Binary.UTF8.Light
import Data.ByteString.Internal

import qualified Paste.State.Old.Content0 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Way content is saved: either in a file or plain as a string
        data Content = File  { filepath :: String }
                     | Plain { plain    :: ByteString }

    |])

$(deriveSerialize ''Content)
instance Version Content where
    mode = extension 1 (Proxy :: Proxy Old.Content)

instance Migrate Old.Content Content where
    migrate (Old.File fp) = File fp
    migrate (Old.Plain text) = Plain $ encode text
