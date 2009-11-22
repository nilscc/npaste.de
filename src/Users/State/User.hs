{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.User ( User (..) ) where

import Happstack.Data
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import qualified Users.State.Old.Login as Login ( Login (..), Password (..) )
import qualified Users.State.Old.User0 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | User data
      data User = User { userLogin    :: String         -- ^ login name
                       , userPassword :: B.ByteString   -- ^ login password as md5 bytestring
                       , userEmail    :: Maybe String   -- ^ (optional) Email
                       }

  |])


-- version + migration stuff

$(deriveSerialize ''User)
instance Version User where
    mode = extension 1 (Proxy :: Proxy Old.User)

instance Migrate Old.User User where
    migrate (Old.User li pw email) =
        User (Login.login li) (C.pack . Login.password $ pw) email
