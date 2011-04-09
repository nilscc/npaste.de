{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Content ( Content (..) ) where

import Happstack.Data

import qualified Codec.Binary.UTF8.Light as UTF8 (decode)

import qualified Paste.State.Old.Content1 as Old

deriveAll [''Show, ''Eq, ''Ord, ''Default] [d|

  -- | Way content is saved: either in a file or plain as a string
  data Content
    = File  { filepath :: String }
    | Plain { plain    :: String }

  |]

deriveSerialize ''Content

instance Version Content where
    mode = extension 2 (Proxy :: Proxy Old.Content)

instance Migrate Old.Content Content where
    migrate (Old.File fp) = File fp
    migrate (Old.Plain text) = Plain $ UTF8.decode text
