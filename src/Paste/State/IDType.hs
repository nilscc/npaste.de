
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.IDType ( IDType(..) ) where

import Happstack.Data
import Paste.State.ID (ID (..))

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Data definitions for custom IDs
        data IDType = DefaultID     -- ^ generate default ID
                    | RandomID Int  -- ^ (min) number of digits
                    | CustomID ID   -- ^ custom ID

    |])

$(deriveSerialize ''IDType)
instance Version IDType -- where
    -- mode = Versioned 1 Nothing
    -- mode = extension 1 (Proxy :: Proxy Old.ID)
