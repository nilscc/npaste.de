
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.UserReply ( UserReply (..) ) where

import Happstack.Data
import Users.State.User ( User )

import qualified Users.State.Old.UserReply0 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
    [d|

        -- | Data definition for replies
        data UserReply = OK User
                       | WrongLogin
                       | WrongPassword
                       | AlreadyExists

    |])

-- migration + version stuff

$(deriveSerialize ''UserReply)
instance Version UserReply where
    mode = extension 1 (Proxy :: Proxy Old.UserReply)

instance Migrate Old.UserReply UserReply where
    migrate (Old.OK user)           = OK user
    migrate (Old.WrongPassword)     = WrongPassword
    migrate (Old.WrongLogin)        = WrongLogin
    migrate (Old.AlreadyExists _)   = AlreadyExists

