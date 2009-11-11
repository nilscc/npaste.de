{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Paste.Post (postHandler) where

import Happstack.Server
import Happstack.State

import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import Control.Monad (msum, mplus, mzero, guard)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust, fromMaybe)
import System.Time (getClockTime, ClockTime(..))

import HSP

import Paste.State
import Users.State
    ( Validate (..)
    , UserReply (..)
    , User(..)
    , Login (..)
    , Password (..)
    )


-- | Remove any trailing white space characters
stripSpaces :: String -> String
stripSpaces = unlines . map (foldr strip "") . lines
  where strip s "" = if isSpace s then "" else [s]
        strip s r  = s : r


postHandler :: ServerPartT IO Response
postHandler = do
    methodM POST
    -- pData <- getData
    case Nothing of
         Just pd -> postData pd
         _ -> badRequest . toResponse $ "Something went wrong. Contact mail (at) n-sch.de if necessary.\n"

postData :: PostData -> ServerPartT IO Response
postData pData = do
    let content  = fromMaybe "" $ cont pData
        filetype = ft pData
        submit   = sub pData
        username = fromMaybe "" $ un  pData
        password = fromMaybe "" $ pwd pData

    -- Get user
    userReply <- query $ Validate (Login username) (Password password)
    let user = case userReply of
                    OK user -> Just user
                    _ -> Nothing

        error | null content =
                  Just "No content given."
              | not (null $ drop (50000) content) =
                  Just "Content size too big (max 100kb)."
              | not (null username) =
                  case userReply of
                       OK _             -> Nothing
                       WrongLogin       -> Just "Wrong login name."
                       WrongPassword    -> Just "Wrong password."
                       _                -> Just "User validation failed."
              | otherwise =
                  Nothing

    case error of
         Just e  -> badRequest $ toResponse e
         Nothing -> do
             -- get time, id and filepath
             now <- liftIO getClockTime
             newId <- query $ GetNewId
             let fp = "pastes/" ++ unId newId
             -- write file & add paste
             liftIO $ writeFile fp content
             update $ AddPaste PasteEntry { date     = now
                                          , content  = File fp
                                          , user     = user
                                          , pId      = NoID
                                          , filetype = filetype
                                          }
             -- send url
             let url = "http://npaste.de/" ++ unId newId
             if submit
                then seeOther url (toResponse url)
                else ok . toResponse $ url ++ "\n"


-- | Define post data
data PostData = PostData { cont :: Maybe String -- ^ Necessary content
                         , un   :: Maybe String -- ^ Username
                         , pwd  :: Maybe String -- ^ Password
                         , ft   :: Maybe String -- ^ Filetype
                         , sub  :: Bool         -- ^ Submit button from form
                         }
-- | Read post data
instance FromData PostData where
    fromData = do
        content <- look "content"  `mplus` return ""
        submit  <- look "submit"   `mplus` return ""
        user    <- look "user"     `mplus` return ""
        passwd  <- look "password" `mplus` return ""
        ft      <- look "filetype" `mplus` return ""
        let makeOpt "" = Nothing
            makeOpt s  = Just s
        return $ PostData { cont = makeOpt content
                          , un   = makeOpt user
                          , pwd  = makeOpt passwd
                          , ft   = makeOpt ft
                          , sub  = not $ null submit
                          }


-- {{{ Old post handler

{-
-- | Take care of POST events
postHandler :: ServerPartT IO Response
postHandler = methodM POST >> msum
    [ do
        -- Get content, asure its not empty
        paste <- getDataFn $ do
            s <- look "content"
            -- Limit size to 50k chars
            guard . null . drop (50000) $ s
            guard . not . null . filter (not . isSpace) $ s
            return . stripSpaces $ s
        guard $ isJust paste

        -- Get login data
        login <- getDataFn $ do
            u <- look "user"
            p <- look "pwd"
            return (u,p)
        user <- case login of
                     Just (user,pwd) -> do
                         resp  <- query $ Validate (Login user) (Password pwd)
                         return $ case resp of
                                       OK user -> Just user
                                       _       -> Nothing
                     _ -> return $ Nothing

        -- Local time
        now   <- liftIO getClockTime

        -- New ID, filepath and content text
        newId <- query $ GetNewId
        let fp   = "pastes/" ++ unId newId
            text = fromJust paste
        
        -- Save the content to file
        liftIO $ writeFile fp text

        update $ AddPaste PasteEntry { date    = Just now
                                     , content = File fp
                                     , user    = user
                                     , pId     = Nothing
                                     }

        -- Handle redirection from a html form
        let url = "http://npaste.de/" ++ unId newId

        submit <- getDataFn $ look "submit"
        if isJust submit
           then seeOther url $ toResponse url
           else ok . toResponse $ url ++ "\n"
    , badRequest . toResponse $ "Something went wrong. Contact mail (at) n-sch.de if necessary.\n"
    ]

-}

-- }}}

