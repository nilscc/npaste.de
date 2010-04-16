{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.PasteSettings0 ( PasteSettings (..) ) where

import Happstack.Data

$(deriveAll [''Show, ''Eq, ''Ord]
  [d|

      data PasteSettings = DefaultPasteSettings
                         | HideNewPastes

  |])

$(deriveSerialize ''PasteSettings)
instance Version PasteSettings
