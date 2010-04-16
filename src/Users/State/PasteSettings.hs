{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.PasteSettings ( PasteSettings (..) ) where

import Happstack.Data
import qualified Users.State.Old.PasteSettings0 as Old

$(deriveAll [''Show, ''Eq, ''Ord]
  [d|

      data PasteSettings = DefaultPasteSettings
                         | HideNewPastes
                         | HideAndRandom

  |])

$(deriveSerialize ''PasteSettings)
instance Version PasteSettings where
    mode = extension 1 (Proxy :: Proxy Old.PasteSettings)

instance Migrate Old.PasteSettings PasteSettings where
    migrate Old.DefaultPasteSettings = DefaultPasteSettings
    migrate Old.HideNewPastes        = HideNewPastes
