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


-- | Handle incoming post data
postHandler :: ServerPartT IO Response
postHandler = do
    methodM POST
    pData <- getData
    case pData of
         Just pd -> postData pd
         _       -> badRequest . toResponse $ "Something went wrong. Contact mail (at) n-sch.de if necessary.\n"

-- | Add post data to database
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
