{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.Register.Register
    (
    -- * Registration functions
      registerNew
    , registerMain
    , registerActivate
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import HSP
import Happstack.Server
import Happstack.State
import Happstack.Util.Mail
import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Rfc2822 (addr_spec)
import System.Random

import qualified Data.Map as M
import qualified Happstack.Auth as Auth

import Paste.View
import Users.State

alphaNum :: [Char]
alphaNum  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

--------------------------------------------------------------------------------
-- | Show main registration form

registerMain :: ServerPart Response
registerMain = do
    login <- getLogin
    nick  <- getDataBodyFn $ look "nick"
    email <- getDataBodyFn $ look "email"

    xmlResponse $ htmlBody login [registerHsp nick email Nothing]

registerHsp :: Maybe String -> Maybe String -> Maybe String -> HSP XML
registerHsp nick email err =
    <div id="main">
        <h1>Registration</h1>
        <p>Please enter your email and desired username (alpha numeric chars only) below. You will receive an email with your activation
            Link as soon as possible. You have 48 hours to activate your account before it gets deleted again.</p>

        <form id="registration-form" action="/?view=register" method="post">
            <% (\e -> <p class="error">Error: <% e %></p>) `fmap` err %>
            <p><span class="desc">Username:</span><input type="text" name="nick" value=(fromMaybe "" nick) /></p>
            <p><span class="desc">Email:</span><input type="text" name="email" value=(fromMaybe "" email) /></p>
            <p><input type="submit" name="submit" value="Register" /></p>
        </form>
    </div>


--------------------------------------------------------------------------------
-- | Register a new user

registerNew :: ServerPart Response
registerNew = do
    methodM POST

    login <- getLogin

    nick <- fromMaybe "" `fmap` getDataBodyFn (look "nick")
    guard $ not (null nick) && all (`elem` alphaNum) nick

    email' <- getDataBodyFn $ look "email"
    let email = case parse addr_spec "registerNew: email" `fmap` email' of
                     Just (Right e) -> e
                     _              -> ""
    guard $ not (null email)

    -- See if that login/email is available
    ud <- query AskUserData

    let exists UserData { userEmail = email' } | email == email' = Just ()
        exists _ = Nothing
        emailExists = not . M.null $ M.mapMaybe exists ud

    loginExists <- query $ Auth.IsUser (Auth.Username nick)

    case (loginExists, emailExists) of

         (True,_) -> xmlResponse $ htmlBody login [registerHsp (Just nick) (Just email) (Just "Login already exists.")]
         (_,True) -> xmlResponse $ htmlBody login [registerHsp (Just nick) (Just email) (Just "Email already exists.")]

         _        -> do
             ak <- update $ AddInactiveUser nick email
             case ak of
                  Just akey -> do liftIO $ sendSimpleMessages "85.14.216.254" "n-sch.de" [activationMail nick (NameAddr (Just nick) email) akey]
                                  xmlResponse $ htmlBody login [newHsp nick email]
                  _ -> xmlResponse $ htmlBody login [registerHsp (Just nick) (Just email) (Just "Inactive user.")]

newHsp :: String -> String -> HSP XML
newHsp nick email =
    <div id="main">
        <h1>Welcome, <% nick %>!</h1>
        <p>An email with your activation key has been send to <em><% email %></em>.</p>
    </div>

activationMail :: String -> NameAddr -> String -> SimpleMessage
activationMail nick to activationkey = SimpleMessage
    { from = [NameAddr (Just "npaste.de") "npaste@n-sch.de"]
    , to = [to]
    , subject = "npaste.de Activation"
    , body = "Hi " ++ nick ++ "!\n\n"
             ++ "Welcome to npaste.de! To activate your account, please follow this link:\n\n"
             ++ "http://npaste.de/?view=register&user=" ++ nick ++ "&activate=" ++ activationkey ++ "\n\n"
             ++ "You have 48 hours to complete this action.\n"
             ++ "If you did not request this email please ignore this email. Do not respond to this email.\n\n"
             ++ "Thank you,\nnpaste.de webmaster"
    }


--------------------------------------------------------------------------------
-- | Activate a user

registerActivate :: ServerPart Response
registerActivate = do
    methodM GET

    login <- getLogin

    nick <- fromMaybe "" `fmap` getDataQueryFn (look "user")
    guard $ not (null nick)

    akey <- fromMaybe "" `fmap` getDataQueryFn (look "activate")
    guard $ not (null akey)

    email <- update $ RemoveInactiveUser nick akey
    case email of

         Just e -> do
             pwd    <- randomString 8
             salted <- liftIO $ Auth.buildSaltAndHash pwd
             user'  <- update $ Auth.AddUser (Auth.Username nick) salted
             case user' of

                  Just Auth.User { Auth.userid = id } -> do
                      update $ AddUser id (UserData e)
                      xmlResponse $ htmlBody login [activationHsp pwd]

                  m -> xmlResponse $ htmlBody login [activationErrorHsp $ Just (show m)]

         _ -> xmlResponse $ htmlBody login [activationErrorHsp $ Just "Invalid user/activation key."]


activationHsp :: String -> HSP XML
activationHsp pwd =
    <div id="main">
        <h1>Activation Complete</h1>
        <p>You can now <a href="/?view=login">login</a> and use your account.</p>
        <p>Your current password is: <strong><% pwd %></strong></p>
        <p>Please change your password as soon as possible in your accounts profile.</p>
    </div>

activationErrorHsp :: Maybe String -> HSP XML
activationErrorHsp err =
    <div id="main">
        <h1>Activation Error</h1>
        <% case err of
                
                Just e  -> <p>An error occurred during activation: <span class="error"><% e %></span></p>
                Nothing -> <p><span class="error">An unknown error occurred during activation.</span> Please contact the webmaster.</p>

        %>
    </div>


-- Helper :)
randomString :: (MonadIO m) => Int -> m String
randomString l
    | l >= 1 = do
        ints <- mapM (const . liftIO $ randomRIO (0,length alphaNum)) [1..l]
        return $ map ((!!) (cycle alphaNum) . abs) ints
    | otherwise   = return ""
