{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}
module Users.State
    ( Users (..)
    , User (..)
    , Login (..)
    , Password (..)
    , AddUser (..)
    , RemoveUser (..)
    , UserReply (..)
    , Validate (..)
    , valid
    ) where

import Happstack.Data
import Happstack.State
import Happstack.Crypto.MD5 (stringMD5)

import Data.ByteString.Lazy.Char8 (pack)
import Data.List (find, delete)
import Data.Maybe (isJust, fromJust)
import Control.Monad (liftM)
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)

-- {{{ Data definitions

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|

      -- | Login ID
      newtype Login = Login { login :: String }
      -- | Password
      newtype Password = Password { password :: String }

      -- | User data
      data User = User { userLogin    :: Login
                       , userPassword :: Password
                       , userEmail    :: Maybe String -- ^ (optional) Email
                       }

      -- | Just a list of users
      newtype Users = Users { users :: [User] }

      -- | Data definition for replies
      data UserReply = OK User
                     | WrongLogin
                     | WrongPassword
                     | AlreadyExists User

  |])

$(deriveSerialize ''Login)
instance Version Login

$(deriveSerialize ''Password)
instance Version Password

$(deriveSerialize ''User)
instance Version User

$(deriveSerialize ''Users)
instance Version Users

$(deriveSerialize ''UserReply)
instance Version UserReply

-- | Dependencies
instance Component Users where
  type Dependencies Users = End
  initialValue = Users []

-- }}} Data definitions


-- | MD5 generation
md5Password = Password . md5Plain . password
md5Plain = stringMD5 . pack

-- | Get a user by his login
getUserByLogin :: Login -> Users -> Maybe User
getUserByLogin l u = find ((== l) . userLogin) (users u)

-- | Validate login and password:
valid :: Login -> Password -> Users -> UserReply
valid name pw userList =
    case getUserByLogin name userList of
         Nothing   -> WrongLogin
         Just user -> if pw == userPassword user
                         then OK user
                         else WrongPassword


-- {{{ State actions

-- | Add a new User
addUser :: Login
        -> Password
        -> Maybe String -- ^ E-Mail
        -> Update Users UserReply
addUser name pw email = do
    userList <- ask
    case valid name pw userList of
         OK user -> do modify $ Users . (:) (User name (md5Password pw) email) . users
                       return $ OK user
         reply   -> return reply

-- | Remove a user
removeUser :: Login
           -> Password
           -> Update Users UserReply
removeUser name pw = do
    userList <- ask
    case valid name (md5Password pw) userList of
         OK user -> do modify $ Users . delete user . users
                       return $ OK user
         reply   -> return reply

-- | Validate a user
validate :: Login
         -> Password
         -> Query Users UserReply
validate name pw = ask >>= return . valid name (md5Password pw)

-- }}} State actions

-- Register methods
$(mkMethods ''Users ['addUser, 'removeUser, 'validate])
