{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Users ( Users (..) ) where

import Happstack.Data
import Happstack.State
import Happstack.Server.HTTP.Types      (Host)
import Happstack.State.ClockTime        (ClockTime (..))

import Users.State.User                 (User (..))
import Users.State.SessionID            (SessionID (..))

import qualified Data.Map as M

import Happstack.Crypto.MD5             (md5)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Users.State.Old.Users0 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | Just a list of users + their session IDs
      data Users = Users { allUsers     :: [User]
                         , sessionIDs   :: M.Map SessionID (User, Host, ClockTime)
                         }

  |])

$(deriveSerialize ''Users)
instance Version Users where
    mode = extension 1 (Proxy :: Proxy Old.Users)

instance Migrate Old.Users Users where
    migrate (Old.Users userList) = Users userList M.empty


-- | MD5 generation
passwordFromString :: String -> B.ByteString
passwordFromString = B.concat . C.toChunks . md5 . C.pack

-- | Dependencies
instance Component Users where
  type Dependencies Users = End
  initialValue = Users [User "root" (passwordFromString "admin") Nothing] M.empty
