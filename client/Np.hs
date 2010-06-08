module Main where

import Control.Monad (when, liftM2)

import Data.List (foldl', insert)
import Data.Maybe (fromMaybe)

import Network.URI (escapeURIString, isAllowedInURI)

import Network.Curl.Code (CurlCode (..))
import Network.Curl.Easy (initialize, setopt, perform)
import Network.Curl.Opts (CurlOption (..))

import System
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.IO
import System.Environment

import Text.JSON
import Text.JSON.Types


--------------------------------------------------------------------------------
-- Save/load user config
--------------------------------------------------------------------------------

configDirectory :: IO FilePath
configDirectory = getAppUserDataDirectory "npaste"

saveUserData :: FilePath    -- ^ Configuration file (!) to use
             -> String      -- ^ User
             -> String      -- ^ Password
             -> IO ()
saveUserData fp user password = do
    createDirectoryIfMissing True (takeDirectory fp)
    exists <- doesFileExist fp
    conf <- if exists
               then do js <- decode `fmap` readFile fp
                       case js of
                            Ok a -> return a
                            _    -> error $ "Malformed config file: " ++ fp
               else return JSNull
    writeFile fp . encode $
        case conf of
             JSObject o -> set_field (set_field o "user" user') "password" password'
             _          -> toJSObject [("user", user'), ("password", password')]

  where user'     = JSString (toJSString user)
        password' = JSString (toJSString password)

-- | Load user data. Returns a pair of username and password if successfull
loadUserData :: FilePath -> IO (Maybe (String,String))
loadUserData fp = do
    exists <- doesFileExist fp
    if exists
       then do fs <- decode `fmap` readFile fp
               case fs of
                    Ok (JSObject o) -> return . maybeResult $ do
                        user <- valFromObj "user" o
                        password <- valFromObj "password" o
                        return (user, password)
                    _ -> return Nothing
       else return Nothing

  where maybeResult (Ok a) = Just a
        maybeResult _      = Nothing


--------------------------------------------------------------------------------
-- Run main programm
--------------------------------------------------------------------------------

main = do
    args <- getArgs

    -- run getOpt
    conf_fp <- (\d -> d ++ [pathSeparator] ++ "user.conf") `fmap` configDirectory
    let (actions, nonOptions, error) = getOpt RequireOrder (options conf_fp) args
    opts <- foldl' (>>=) (return $ Options Nothing Nothing Nothing [] Nothing Nothing (Just conf_fp)) actions

    let file = case nonOptions of
                    (file:_) -> file
                    _        -> ""

        ft = case filetype opts of
                  Just f -> f
                  Nothing -> case takeExtension file of
                                  (extSeparator : ext) -> ext
                                  ext                  -> ext

        desc = case replies opts of
                    []   -> description opts
                    list -> Just $ "Reply to "
                                    ++ (foldr (\id rest -> "/" ++ id ++ "/" ++ rest) "" list)
                                    ++ (maybe "" (". " ++) $ description opts)

        opts' = opts { filetype = Just ft, description = desc }

    -- get file content/read from stdin
    exists <- doesFileExist file
    text <- if exists
               then readFile file
               else getContents

    -- check for username/password
    opts'' <- case opts' of
                  Options { user = Just un, password = Nothing } -> do
                      putStrLn $ "Enter password for username '" ++ un ++ "':"
                      hFlush stdout
                      pw <- getLine
                      return opts' { password = Just pw }
                  Options { user = Nothing, password = Nothing, config = Just cfg } -> do
                      ud <- loadUserData cfg
                      case ud of
                           Just (un,pw) -> do
                               putStrLn $ "Using username '" ++ un ++ "' from config file"
                               return opts' { user = Just un, password = Just pw }
                           Nothing -> do
                               return opts'
                  _ -> do return opts'

    -- post it! combine replies + description
    post opts'' text
    return ()



-- | Post data with curl
post :: Options -> String -> IO CurlCode
post opts text = do
    curl <- initialize
    mapM_ (setopt curl) [ CurlURL "http://npaste.de"
                        , CurlPost True 
                        , CurlPostFields $ [ "content="     ++ escape text
                                           , "filetype="    ++ escape (fromMaybe "" $ filetype opts)
                                           , "id="          ++ escape (fromMaybe "" $ pId opts)
                                           , "description=" ++ escape (fromMaybe "" $ description opts)
                                           , "user="        ++ escape (fromMaybe "" $ user opts)
                                           , "password="    ++ escape (fromMaybe "" $ password opts)
                                           ]
                        ]
    perform curl

  where escape = escapeURIString $ isAllowedInURI &&& (not . (`elem` " !\"#$%&'()*+,-./0123456789:;<=>?@"))
        (&&&) = liftM2 (&&)

-- | Options
data Options = Options { filetype       :: Maybe String
                       , pId            :: Maybe String
                       , description    :: Maybe String
                       , replies        :: [String]
                       , user           :: Maybe String
                       , password       :: Maybe String
                       , config         :: Maybe FilePath
                       }

-- | Define options
options conf_fp =
    [ Option "f" ["filetype"]
          (ReqArg (\arg opt -> return opt { filetype = Just arg })
                  "FILETYPE")
          "Default highlighting filetype/language"
    , Option "i" ["id"]
          (ReqArg (\arg opt -> return opt { pId = Just arg })
                  "ID")
          "Define random or custom IDs. Use 'random' or 'rand' for random ID, anything else for custom ID"
    , Option "d" ["description"]
          (ReqArg (\arg opt -> return opt { description = Just arg}) "DESCRIPTION")
          "Add a description to your paste"
    , Option "r" ["reply"]
          (ReqArg (\arg opt -> return opt { replies = insert arg $ replies opt }) "ID")
          "Reply to a paste with ID"
    , Option "h" ["help"]
          (NoArg (const $ do hPutStrLn stderr $ usageInfo "npaste.de client v0.1\n\nUseage:" (options conf_fp)
                             exitWith ExitSuccess))
          "Show help"

    , Option "u" ["user"]
          (ReqArg (\arg opt -> return opt { user = Just arg }) "USERNAME")
          "Paste with your username"
    , Option "p" ["password"]
          (ReqArg (\arg opt -> return opt { password = Just arg }) "PASSWORD")
          "Use your password with your username"

    , Option "c" ["config"]
          (ReqArg (\arg opt -> return opt { config = Just arg }) "CONFIG")
          ("Use custom config file (current: " ++ conf_fp ++ ")")
    , Option "s" ["save"]
          (NoArg (\opt -> do let fp = fromMaybe conf_fp (config opt)
                             putStrLn $ "Saving config file: " ++ fp
                             let (u,p) = case (user opt, password opt) of
                                              (Just un, Just pw) -> (un, pw)
                                              _                  -> error "No user/password!"
                             saveUserData fp u p
                             exitWith ExitSuccess))
          "Save current user/password to config file"
    ]
