module Paste.Post.Register
    ( register
    , activateUser
    ) where

import Control.Monad                (mzero, liftM)
import Control.Monad.Trans          (liftIO)
import Control.Monad.Error          (ErrorT (..), throwError, when, unless)

import Text.ParserCombinators.Parsec            (parse)
import Text.ParserCombinators.Parsec.Rfc2822    (address)

import Happstack.Server             (ServerPartT (..), Response, ServerPart (..), askRq, Request (..))
import Happstack.State              (query, update)
import Happstack.Util.Mail          (SimpleMessage (..), sendSimpleMessages, NameAddr (..))

import Paste.Types.Register         (RegisterError (..))
import Users.State                  ( UserExists (..)
                                    , AddInactiveUser (..)
                                    , ActivateUser (..)
                                    , RemoveInactiveUsers (..)
                                    , User (..)
                                    , UserReply (..)
                                    , UserOfLogin (..)
                                    , AddRegisteredHost (..)
                                    , ClearRegisteredHosts (..)
                                    )

type Nick           = String
type Email          = String
type ActivationKey  = String
type PlainPassword  = String

-- | Register a new user
register :: Nick -> Email -> ErrorT RegisterError (ServerPartT IO) User
register nick email
    | null nick  = throwError $ InvalidName
    | null email = throwError $ InvalidEmail
    | otherwise  = do
        update $ RemoveInactiveUsers
        -- remove registered hosts older than 24hours
        update $ ClearRegisteredHosts 24
        -- generate activation key
        user <- update $ AddInactiveUser nick email
        case user of
             OK user@(InactiveUser login email akey _) -> do
                 -- send email :)
                 liftIO $ sendSimpleMessages "85.14.216.254" "n-sch.de" [activationMail login (NameAddr (Just login) email) akey]

                 -- remember host
                 rq <- askRq
                 update . AddRegisteredHost $ rqPeer rq

                 return user
             other -> throwError . OtherRegisterError $ show other


-- | Activate a new user
activateUser :: Nick -> ActivationKey -> ServerPart (Maybe PlainPassword)
activateUser nick akey = do
    update $ RemoveInactiveUsers
    user <- query $ UserOfLogin nick
    case user of
         Just user -> update $ ActivateUser user akey
         Nothing   -> return Nothing


activationMail :: Nick -> NameAddr -> ActivationKey -> SimpleMessage
activationMail nick to activationkey = SimpleMessage
    { from = [NameAddr (Just "Npaste.de") "npaste@n-sch.de"]
    , to = [to]
    , subject = "npaste.de Activation"
    , body = "Hi " ++ nick ++ "!\n\n"
             ++ "Welcome to npaste.de! To activate your account, please follow this link:\n\n"
             ++ "http://npaste.de/?view=register&user=" ++ nick ++ "&activate=" ++ activationkey ++ "\n\n"
             ++ "You have 48 hours to complete this action.\n"
             ++ "If you did not request this email please ignore this email. Do not respond to this email.\n\n"
             ++ "Thank you,\nnpaste.de webmaster"
    }
