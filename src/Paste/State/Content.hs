{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    TypeSynonymInstances, UndecidableInstances
    #-}

module Paste.State.Content ( Content (..) ) where


import Happstack.Data

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Way content is saved: either in a file or plain as a string
        data Content = File { filepath :: String }
                     | Plain { plain   :: String }

    |])

$(deriveSerialize ''Content)
instance Version Content
