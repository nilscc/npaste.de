{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.ID ( ID(..) ) where


import Happstack.Data

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Default ID definition
        data ID = ID { unId :: String }
                | NoID

    |])

$(deriveSerialize ''ID)
instance Version ID where
    mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.ID)
