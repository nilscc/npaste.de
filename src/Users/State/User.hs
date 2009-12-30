{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}

module Users.State.User ( User (..) ) where

import Happstack.Data
import Happstack.State.ClockTime        (ClockTime (..))

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe                               (fromMaybe)

import qualified Users.State.Old.Login as Login ( Login (..), Password (..) )
import qualified Users.State.Old.User1 as Old

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | User data
      data User = User { userLogin    :: String         -- ^ login name
                       , userPassword :: B.ByteString   -- ^ login password as md5 bytestring
                       , userEmail    :: String         -- ^ Email
                       }
                | InactiveUser { userLogin'         :: String -- ^ login name
                               , userEmail'         :: String -- ^ email
                               , activationkey      :: String -- ^ activation key
                               , registrationDate   :: ClockTime -- ^ time of their registration, inactive users get removed after 48 hours
                               }

  |])


-- version + migration stuff

$(deriveSerialize ''User)
instance Version User where
    mode = extension 2 (Proxy :: Proxy Old.User)

instance Migrate Old.User User where
    migrate (Old.User li pw email) = User li pw (fromMaybe "" email)
