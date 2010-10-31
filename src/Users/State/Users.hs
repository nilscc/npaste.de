{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Users
    ( Users (..)
    ) where

import Happstack.Data
import Happstack.State
import Happstack.Server.Internal.Types
import Happstack.State.ClockTime

import Users.State.UserData

import qualified Happstack.Auth             as Auth
import qualified Data.Map                   as M

-- Migration stuff
import qualified Users.State.Old.Users2 as Old

type ActivationKey  = String

-- | (user name, email)
type Login          = String
type Email          = String
type InactiveUser   = (Login, Email, ClockTime)

$(deriveAll [''Show, ''Eq, ''Ord]
  [d|

      -- | Just a list of users + their session IDs
      data Users = Users { userData         :: M.Map Auth.UserId UserData
                         , sessionData      :: M.Map Auth.SessionKey (Host, ClockTime)
                         , registeredHosts  :: M.Map Host ClockTime
                         , inactiveUsers    :: M.Map ActivationKey InactiveUser
                         }

  |])

$(deriveSerialize ''Users)
instance Version Users where
    mode = extension 3 (Proxy :: Proxy Old.Users)

instance Migrate Old.Users Users where
    -- We have to start all over anyway, so we can "forget" all users/sessions
    migrate (Old.Users _ _ hosts) = Users M.empty M.empty hosts M.empty

-- | Dependencies
instance Component Users where
  type Dependencies Users = End
  initialValue = Users M.empty M.empty M.empty M.empty
