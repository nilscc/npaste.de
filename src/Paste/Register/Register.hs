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
import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Rfc2822 (addr_spec)
import System.Random

import qualified Data.Map                       as M
import qualified Network.SMTP.Simple            as N
import qualified Happstack.Auth.Internal        as Auth
import qualified Happstack.Auth.Internal.Data   as AuthD

import Paste.View
import Users.State

alphaNum :: [Char]
alphaNum  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

--------------------------------------------------------------------------------
-- | Show main registration form

registerMain :: ServerPart Response
registerMain = do

    nick  <- fmap (either (const Nothing) Just) . getDataFn . body $ look "nick"
    email <- fmap (either (const Nothing) Just) . getDataFn . body $ look "email"

    htmlBody [registerHsp nick email Nothing]

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

    nick <- either (const "") id `fmap` getDataFn (body $ look "nick")
    guard $ not (null nick) && all (`elem` alphaNum) nick

    email' <- getDataFn . body $ look "email"
    let email = case parse addr_spec "registerNew: email" `fmap` email' of
                     Right (Right e) -> e
                     _               -> ""
    guard $ not (null email)

    -- See if that login/email is available
    ud <- query AskUserData

    let exists UserData { userEmail = email' } | email == email' = Just ()
        exists _ = Nothing
        emailExists = not . M.null $ M.mapMaybe exists ud

    loginExists <- query $ Auth.IsUser (AuthD.Username nick)

    case (loginExists, emailExists) of

         (True,_) -> htmlBody [registerHsp (Just nick) (Just email) (Just "Login already exists.")]
         (_,True) -> htmlBody [registerHsp (Just nick) (Just email) (Just "Email already exists.")]

         _        -> do
             ak <- update $ AddInactiveUser nick email
             case ak of
                  Just akey -> do liftIO $ N.sendSimpleMessages (appendFile "mailerr.log") "10.8.0.1" "npaste.de"
                                                                [activationMail nick (N.NameAddr (Just nick) email) akey]
                                  htmlBody [newHsp nick email]
                  _ -> htmlBody [registerHsp (Just nick) (Just email) (Just "Inactive user.")]

newHsp :: String -> String -> HSP XML
newHsp nick email =
    <div id="main">
        <h1>Welcome, <% nick %>!</h1>
        <p>An email with your activation key has been send to <em><% email %></em>.</p>
    </div>

activationMail :: String -> N.NameAddr -> String -> N.SimpleMessage
activationMail nick to activationkey = N.SimpleMessage
    { N.from = [N.NameAddr (Just "npaste.de") "noreply@npaste.de"]
    , N.to = [to]
    , N.subject = "npaste.de Activation"
    , N.body = "Hi " ++ nick ++ "!\n\n"

             ++ "Welcome to npaste.de! To activate your account, please follow this link:\n\n"

             ++ "http://npaste.de/?view=register&user=" ++ nick ++ "&activate=" ++ activationkey ++ "\n\n"

             ++ "You have 48 hours to complete this action.\n"
             ++ "If you did not request this email please ignore this email. Do not respond to this email.\n\n"

             ++ "Thank you for using npaste.de,\n"
             ++ "  npaste.de webmaster"
    }


--------------------------------------------------------------------------------
-- | Activate a user

registerActivate :: ServerPart Response
registerActivate = do
    methodM GET

    nick <- either (const "") id `fmap` getDataFn (queryString $ look "user")
    guard $ not (null nick)

    akey <- either (const "") id `fmap` getDataFn (queryString $ look "activate")
    guard $ not (null akey)

    email <- update $ RemoveInactiveUser nick akey
    case email of

         Just e -> do
             pwd         <- randomString 8
             Just salted <- liftIO $ Auth.buildSaltAndHash pwd
             user'       <- update $ Auth.AddUser (AuthD.Username nick) salted
             case user' of

                  Just AuthD.User { AuthD.userid = id } -> do
                      update $ AddUser id (UserData e Nothing DefaultPasteSettings)
                      htmlBody [activationHsp pwd]

                  m -> htmlBody [activationErrorHsp $ Just (show m)]

         _ -> htmlBody [activationErrorHsp $ Just "Invalid user/activation key."]


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
