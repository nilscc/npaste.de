{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses
    #-}

module Paste.State.PasteDB
    ( PasteDB
    ) where

import Happstack.Data.IxSet

import Paste.State.PasteEntry
import Paste.State.NewTypes

inferIxSet "PasteDB" ''PasteEntry 'noCalcs
  [ ''PUser
  , ''PId
  , ''PDate
  , ''PHash
  , ''PHide
  , ''PTags
  ]
