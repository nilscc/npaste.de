module Paste.Post.NewPaste
    ( newPasteHandler
    ) where

import Happstack.Server
import Happstack.State

import Control.Monad.Trans                          (liftIO)
import Control.Monad.Error

import Data.Char                                    (isSpace, toLower)
import Data.Maybe                                   (fromJust, isJust, isNothing, fromMaybe)

import System.Directory                             (createDirectoryIfMissing)
import System.FilePath                              (pathSeparator)
import System.Time

import HSP
import Text.Highlighting.Kate                       (languagesByExtension, languages)

import Happstack.Crypto.MD5                         (md5)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C

import Paste.View.Index (showIndex')
import Paste.State
import Paste.Types                                  (PostError (..))

import Users.State
    ( Validate' (..)
    , UserReply (..)
    , User (..)
    )

md5string :: String -> B.ByteString
md5string = B.concat . C.toChunks . md5 . C.pack

-- | Remove any trailing white space characters
stripSpaces :: String -> String
stripSpaces = unlines . map (foldr strip "") . lines
  where strip s "" = if isSpace s then "" else [s]
        strip s r  = s : r


-- | Handle incoming post data
newPasteHandler :: ServerPart Response
newPasteHandler = do
    methodM POST
    -- check if we want to reply
    reply  <- getDataBodyFn $ look "reply"
    guard $ isNothing reply
    errorT <- runErrorT post

    -- check if we used the html form or a tiny url
    mSubmit <- getDataBodyFn $ look "submit"
    mFiletype <- getDataBodyFn $ look "filetype"
    let submit = not . null $ fromMaybe "" mSubmit
        isTiny | (fromMaybe "" mFiletype) `elem` tinyIds = True
               | otherwise = False

    case errorT of
         Left e | submit    -> showIndex' $ Just (show e)
                | otherwise -> badRequest . toResponse $ show e ++ "\n"
         Right url | submit && isTiny -> seeOther ("/" ++ url ++ "/plain") $ toResponse ("http://npaste.de/" ++ url ++ "/plain")
                   | submit           -> seeOther ("/" ++ url ++ "/")      $ toResponse ("http://npaste.de/" ++ url ++ "/")
                   | otherwise        -> ok . toResponse $ "http://npaste.de/" ++ url ++ "/\n"

type Url = String

-- | Try to post data, throw PostError if anything goes wrong.
post :: ErrorT PostError (ServerPartT IO) Url
post = do

    -- simple check for spam
    mSpam <- getDataBodyFn $ look "email" -- email field = spam!
    unless (null $ fromMaybe "" mSpam) (throwError IsSpam)

    -- check if host is allowed to post
    update $ ClearKnownHosts 60
    rq      <- askRq
    let peer = rqPeer rq
    ctime   <- liftIO getClockTime
    htime   <- query $ GetClockTimeByHost 10 peer
    case htime of
         Just time -> throwError . MaxPastes $ let time' = addToClockTime noTimeDiff { tdHour = 1 } time
                                               in normalizeTimeDiff $ diffClockTimes time' ctime
         _ -> return ()

    -- get and validate our content
    mContent <- getDataBodyFn $ look "content"
    let content     = stripSpaces $ fromMaybe "" mContent
        maxSize     = 200
        md5content  = md5string content
    when (null content)                           (throwError NoContent)
    unless (null $ drop (maxSize * 1000) content) (throwError $ ContentTooBig maxSize)

    -- get description
    mDescription <- getDataBodyFn $ look "description"
    let desc = case mDescription of
                    d@(Just a) | not (null a) -> d
                    _ -> Nothing
    unless (null $ drop 300 (fromMaybe "" desc)) (throwError $ DescriptionTooBig 300)

    -- get and validate user
    mUser       <- getDataBodyFn $ look "user"
    mPassword   <- getDataBodyFn $ look "password"
    let user'   = fromMaybe "" mUser
        passwd' = fromMaybe "" mPassword
    userReply <- query $ Validate' user' passwd'
    validUser <- case userReply of
                      OK user                        -> return $ Just user
                      _ | null user' && null passwd' -> return Nothing
                      WrongLogin                     -> throwError WrongUserLogin
                      WrongPassword                  -> throwError WrongUserPassword
                      _                              -> throwError $ OtherPostError "User validation failed."

    -- check if the content is already posted by our user
    peByMd5 <- query $ GetPasteEntryByMd5sum validUser md5content
    case peByMd5 of
         Just pe -> throwError $ MD5Exists pe
         _       -> return ()

    -- get id
    mId       <- getDataBodyFn $ look "id"
    mIdType   <- getDataBodyFn $ look "id-type"
    let idT' | null (fromMaybe "" mIdType) = map toLower $ fromMaybe "" mId
             | otherwise = map toLower $ fromMaybe "" mIdType
        idT | idT' `elem` defaultIds = DefaultID
            | idT' `elem` randomIds  = RandomID 10
            | otherwise              = CustomID . ID $ fromMaybe "" mId
    id <- query $ GenerateId validUser idT
    when (NoID == id) (throwError InvalidID)

    -- get filetype
    mFiletype <- getDataBodyFn $ look "filetype"

    -- save to file
    let dir      = "pastes" ++ (maybe "" ([pathSeparator] ++) $ liftM userLogin validUser)
        filepath = dir ++ [pathSeparator] ++ unId id
    liftIO $ do createDirectoryIfMissing True dir
                writeFile filepath content
    idR <- update $ AddPaste PasteEntry { date = ctime
                                        , content = File filepath
                                        , user = validUser
                                        , pId = id
                                        , filetype = mFiletype
                                        , md5hash = md5content
                                        , postedBy = peer
                                        , description = desc
                                        }

    -- add to known hosts
    update $ AddKnownHost peer

    -- return url
    return $ unId id
