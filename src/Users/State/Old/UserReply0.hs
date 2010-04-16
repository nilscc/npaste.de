
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.UserReply0 ( UserReply (..) ) where

import Happstack.Data
import Users.State.Old.User2 ( User )


$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Data definition for replies
        data UserReply = OK User
                       | WrongLogin
                       | WrongPassword
                       | AlreadyExists User

    |])

$(deriveSerialize ''UserReply)
instance Version UserReply
