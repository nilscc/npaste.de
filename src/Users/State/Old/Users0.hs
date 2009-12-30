{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.Old.Users0 ( Users (..) ) where

import Happstack.Data
import Happstack.State
import Users.State.Old.User1 ( User (..) )

import Happstack.Crypto.MD5 (md5)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | Just a list of users
      newtype Users = Users { users :: [User] }

  |])

$(deriveSerialize ''Users)
instance Version Users

-- | MD5 generation
passwordFromString :: String -> B.ByteString
passwordFromString = B.concat . C.toChunks . md5 . C.pack

-- | Dependencies
instance Component Users where
  type Dependencies Users = End
  initialValue = Users [User "root" (passwordFromString "admin") Nothing]
