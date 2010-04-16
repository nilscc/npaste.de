{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.Users2 ( Users (..) ) where

import Happstack.Data
import Happstack.State
import Happstack.Server.HTTP.Types hiding (Version (..))
import Happstack.State.ClockTime

import Users.State.Old.SessionID

import qualified Data.Map as M

import Happstack.Crypto.MD5
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Users.State.Old.Users1 as Old
import qualified Users.State.Old.User2 as OldU

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | Just a list of users + their session IDs
      data Users = Users { allUsers         :: [OldU.User]
                         , sessionIDs       :: M.Map SessionID (OldU.User, Host, ClockTime)
                         , registeredHosts  :: M.Map Host ClockTime
                         }

  |])

$(deriveSerialize ''Users)
instance Version Users where
    mode = extension 2 (Proxy :: Proxy Old.Users)

instance Migrate Old.Users Users where
    migrate (Old.Users userList sessionIds) = Users userList sessionIds M.empty


-- | MD5 generation
passwordFromString :: String -> B.ByteString
passwordFromString = B.concat . C.toChunks . md5 . C.pack

-- | Dependencies
instance Component Users where
  type Dependencies Users = End
  initialValue = Users [OldU.User "root" (passwordFromString "admin") "npaste@n-sch.de"] M.empty M.empty

$(mkMethods ''Users [])
