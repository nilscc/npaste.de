-- {-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Paste.Post
    ( postHandler
    , PasteResponse(..)
    ) where

import Happstack.Server
import Happstack.State

import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import Control.Monad (msum, mplus, mzero, guard)

import Data.Char (isSpace, toLower)
import Data.Maybe (fromJust, isJust, fromMaybe)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (pathSeparator)
import System.Time (getClockTime, ClockTime(..))

import HSP
import Text.Highlighting.Kate (languagesByExtension, languages)

import Paste.ViewIndex (showIndex')
import Paste.State
import Paste.Types
    ( PasteResponse (..)
    , PostData (..)
    , ShowOnIndex (..)
    )

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
         Just pd -> post pd
         _       -> badRequest . toResponse $ "Something went wrong. Contact mail (at) n-sch.de if necessary.\n"

-- | Add post data to database
post :: PostData -> ServerPartT IO Response
post pData = do
    let content     = stripSpaces  $ cont pData
        filetype    = ft pData
        submit      = sub pData
        username    = fromMaybe "" $ un  pData
        password    = fromMaybe "" $ pwd pData
        idT         = idType pData
        md5content  = md5string content

    -- validate that there is no other paste entry with the same md5
    peByMd5 <- query $ GetPasteEntryByMd5sum md5content
    -- Generate a new ID
    id  <- query $ GenerateId idT

    -- Get user
    userReply <- query $ Validate (Login username) (Password password)
    let user' = case userReply of
                    OK user -> Just user
                    _       -> Nothing
        maxSize = 50 -- max size in kb
        response | null content =
                     EmptyContent
                 | not (null $ drop (maxSize * 1000) content) =
                     ContentTooBig maxSize
                 | not (null username) =
                     case userReply of
                          OK _             -> NoError idT
                          WrongLogin       -> WrongUserLogin
                          WrongPassword    -> WrongUserPassword
                          _                -> Other
                 | id == NoID =
                     InvalidID
                 | otherwise =
                     case peByMd5 of
                          Just pe | user' == user pe -> MD5Exists pe
                          _ -> NoError idT

    case response of
         MD5Exists pe -> let url = "http://npaste.de/" ++ unId (pId pe) ++ "/"
                         in  showUrl submit url
         NoError _ -> do
             -- get time, id and filepath
             let folder = "pastes"
                 fp     = folder ++ [pathSeparator] ++ unId id
                 url    = "http://npaste.de/" ++ unId id ++ "/"
             now <- liftIO getClockTime

             -- write file & add paste
             idR <- update $ AddPaste PasteEntry { date     = now
                                                 , content  = File fp
                                                 , user     = user'
                                                 , pId      = id
                                                 , filetype = filetype
                                                 , md5hash  = md5content
                                                 }

             case idR of
                  NoID -> badRequest . toResponse . show $ Other
                  ID _ -> do -- save paste to file
                             liftIO $ do createDirectoryIfMissing True folder
                                         writeFile fp content
                             showUrl submit url


         _ -> if submit
                 then showIndex' $ ShowOnIndex (Just pData) (Just response)
                 else badRequest . toResponse $ show response ++ "\n"

-- handle http post form / curl post
showUrl :: Bool -> String -> ServerPartT IO Response
showUrl submit url = if submit
                        then seeOther url . toResponse $ url
                        else ok           . toResponse $ url ++ "\n"
-- | Read post data
instance FromData PostData where
    fromData = do
        content <- look "content"
        submit  <- look "submit"   `mplus` return ""
        user    <- look "user"     `mplus` return ""
        passwd  <- look "password" `mplus` return ""
        ft      <- look "filetype" `mplus` return ""
        id      <- look "id"       `mplus` return ""
        idT     <- look "id-type"  `mplus` return ""
        let makeOpt "" = Nothing
            makeOpt s  = Just s
            id' | idT' `elem` defaultIds = DefaultID
                | idT' `elem` randomIds  = RandomID 10
                | otherwise              = CustomID $ ID id
              where idT' | null idT  = map toLower id
                         | otherwise = map toLower idT

        return $ PostData { cont    = content
                          , un      = makeOpt user
                          , pwd     = makeOpt passwd
                          , ft      = makeOpt ft
                          , idReq   = makeOpt id
                          , idType  = id'
                          , sub     = not $ null submit
                          }
