{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.Profile.Profile
    ( profileShow
    , profileUpdate
    , profileActivateEmail
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import HSP
import Happstack.Server
import Happstack.State
import Happstack.Util.Mail
import System.Random

import qualified Happstack.Auth as Auth

import Paste.View
import Paste.Types
import Users.State

data Profile = Profile
    { email         :: String
    , emailOpts     :: Maybe (HSP XML)
    , pwdOpts       :: Maybe (HSP XML)
    , pasteSettings :: PasteSettings
    , pasteOpts     :: Maybe (HSP XML)
    }

--------------------------------------------------------------------------------
-- | Show main profile

profileShow :: ServerPart Response
profileShow = do

    login <- getLogin
    case login of

         LoggedInAs skey -> do

             sdata <- query $ Auth.GetSession skey
             case sdata of

                  Just Auth.SessionData { Auth.sesUid = uid } -> do

                      -- pwdCur <- getDataBodyFn (look "pwd-cur")

                      Just ud <- query $ UserDataByUserId uid

                      emailOpt <- case userEmailRequested ud of
                                       Just (emailReq,_) -> return $ Just <p class="info">An activation key has been sent to <em><% emailReq %></em>.</p>
                                       _                 -> return Nothing
                      htmlBody [profileHsp $ Profile (userEmail ud)
                                                     emailOpt
                                                     Nothing
                                                     (defaultPasteSettings ud)
                                                     Nothing
                               ]

                  _ -> mzero

         _ -> mzero



--------------------------------------------------------------------------------
-- | Update our profile on POST data

profileUpdate :: ServerPart Response
profileUpdate = do

    methodM POST

    login  <- getLogin

    Auth.SessionData { Auth.sesUid = uid, Auth.sesUsername = Auth.Username uname } <- case login of

         LoggedInAs skey -> do
             sd <- query $ Auth.GetSession skey
             case sd of
                  Just sd -> return sd
                  _       -> mzero

         _               -> mzero

    pwdCur <- getDataBodyFn $ look "pwd-cur"
    pwdNew <- getDataBodyFn $ look "pwd-new"
    pwdCon <- getDataBodyFn $ look "pwd-confirm"

    -- Change password if requested
    pwdChange <- case (pwdCur, pwdNew, pwdCon) of

         (Just cur, Just new, Just con)
            | not (null cur || null new || null con) && new == con -> do

                succ <- Auth.changePassword uname cur new

                if succ
                   then return $ Just <p class="success">Password changed.</p>
                   else return $ Just <p class="error">Wrong password.</p>

            | not (null cur || null new || null con) -> do

                return $ Just <p class="error">New password doesn't match confirmed password.</p>

         _ -> return Nothing

    Just ud <- query $ UserDataByUserId uid

    -- Change email settings
    newEmail <- fromMaybe "" `fmap` getDataBodyFn (look "email")
    emailOpt <- if null newEmail || newEmail == userEmail ud

                   then case userEmailRequested ud of
                             Just (emailReq,_) -> return $ Just <p class="info">An activation key has been sent to <em><% emailReq %></em>.</p>
                             _                 -> return Nothing

                   else do
                       akey <- randomString 30
                       liftIO $ sendSimpleMessages "10.8.0.1" "npaste.de" [activationMail uname (NameAddr (Just uname) newEmail) akey]
                       update $ SetRequestedEmail uid newEmail akey
                       return $ Just <p class="success">An activation key has been send to your new email address.</p>

    -- Change default paste settings
    newPaste <- getDataBodyFn $ look "new-pastes"
    defPaste <- case newPaste of

                     Just t
                        | t == "default" && defaultPasteSettings ud /= DefaultPasteSettings -> do
                            update $ SetDefaultPasteSetting uid DefaultPasteSettings
                            return $ Just <p class="success">Settings changed.</p>

                        | t == "hidden"  && defaultPasteSettings ud /= HideNewPastes -> do
                            update $ SetDefaultPasteSetting uid HideNewPastes
                            return $ Just <p class="success">Settings changed.</p>

                     _ -> return Nothing

    Just ud <- query $ UserDataByUserId uid

    htmlBody [ profileHsp $ Profile (userEmail ud)
                                    emailOpt
                                    pwdChange
                                    (defaultPasteSettings ud)
                                    defPaste
             ]


activationMail :: String -> NameAddr -> String -> SimpleMessage
activationMail nick to activationkey = SimpleMessage
    { from = [NameAddr (Just "npaste.de") "noreply@npaste.de"]
    , to = [to]
    , subject = "npaste.de: Activate your email address"
    , body = "Hi " ++ nick ++ "!\n\n"

             ++ "To activate your new email address, follow this link:\n\n"

             ++ "http://npaste.de/?view=profile&activate-email=" ++ activationkey ++ "\n\n"

             ++ "If you did not request this email please ignore this email. Do not respond to this email.\n\n"

             ++ "Thank you for using npaste.de,\n"
             ++ "  npaste.de webmaster"
    }


--------------------------------------------------------------------------------
-- | Activate a new email address

profileActivateEmail :: ServerPart Response
profileActivateEmail = do

    methodM GET

    akey <- fromMaybe "" `fmap` getDataQueryFn (look "activate-email")
    guard $ not (null akey)

    login <- getLogin
    case login of

         LoggedInAs skey -> do
             sdata <- query $ Auth.GetSession skey
             case sdata of

                  Just Auth.SessionData { Auth.sesUid = uid } -> do

                      succ <- update $ SetNewEmail uid akey

                      if succ
                         then htmlBody [profileEmailActivatedHsp]
                         else htmlBody [profileEmailInvalidActivationKeyHsp]

                  _ -> mzero
         _ -> mzero


profileEmailActivatedHsp :: HSP XML
profileEmailActivatedHsp =
    <div id="main">
        <h1>Email activated</h1>
        <p>Your new email has been activated. You can change the rest of your settings in <a href="/?view=profile">your profile</a>.</p>
    </div>

profileEmailInvalidActivationKeyHsp :: HSP XML
profileEmailInvalidActivationKeyHsp =
    <div id="main">
        <h1>Invalid activation key</h1>
        <p>Your activation key is invalid. Make sure you copied the correct link.</p>
    </div>

--------------------------------------------------------------------------------
-- | HSP

profileHsp :: Profile -> HSP XML
profileHsp profile =
    <div id="main">
        <h1>My profile</h1>

        <form id="profile-form" method="post" action="/?view=profile">
            <p>To change your password, please enter your current and the new password below.</p>
            <% pwdOpts profile %>
            <p><span class="desc">Current password:</span><input type="password" name="pwd-cur" /><br />
               <span class="desc">New password:</span><input type="password" name="pwd-new" /><br />
               <span class="desc">Confirm new password:</span><input type="password" name="pwd-confirm" /></p>

            <p>If you change your current email address npaste.de will send you another activation key to your new email address.
               Until then npaste.de will use your current email.</p>
            <% emailOpts profile %>
            <p><span class="desc">Email:</span><input type="text" name="email" value=(email profile) /></p>

            <p>Your default settings for new pastes:</p>
            <% pasteOpts profile %>
            <p class="radio">
                <% makeRadio "default" (== DefaultPasteSettings) %> Use default IDs<br />
                <% makeRadio "hidden"  (== HideNewPastes)        %> Hide new pastes and use random IDs
            </p>

            <p><input type="submit" name="submit" value="Submit changes" /> <input type="reset" name="reset" value="Reset changes" /></p>
        </form>

    </div>

  where makeRadio val pred
            | pred (pasteSettings profile) = <input type="radio" name="new-pastes" value=val checked="checked" />
            | otherwise                    = <input type="radio" name="new-pastes" value=val />


-- Helper :)
alphaNum :: [Char]
alphaNum  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

randomString :: (MonadIO m) => Int -> m String
randomString l
    | l >= 1 = do
        ints <- mapM (const . liftIO $ randomRIO (0,length alphaNum)) [1..l]
        return $ map ((!!) (cycle alphaNum) . abs) ints
    | otherwise   = return ""
