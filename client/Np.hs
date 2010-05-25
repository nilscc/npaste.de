module Main where

import Control.Monad (when, liftM2)

import Data.List (foldl', insert)
import Data.Maybe (fromMaybe)

import Network.URI (escapeURIString, isAllowedInURI)

import Network.Curl.Code (CurlCode (..))
import Network.Curl.Easy (initialize, setopt, perform)
import Network.Curl.Opts (CurlOption (..))

import System (getProgName, exitWith, ExitCode(..))
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, extSeparator)
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import System.Environment (getArgs)



main = do
    args <- getArgs

    -- run getOpt
    let (actions, nonOptions, error) = getOpt RequireOrder options args
    opts <- foldl' (>>=) (return $ Options Nothing Nothing Nothing [] Nothing Nothing) actions

    let file = case nonOptions of
                    (file:_) -> file
                    _ -> ""

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

    -- check for username/password
    opts'' <- case opts' of
                  Options { user = Just un, password = Nothing} -> do
                      putStrLn $ "Enter password for username '" ++ un ++ "':"
                      hFlush stdout
                      pw <- getLine
                      return opts' { password = Just pw }
                  _ -> do return opts'

    -- get file content/read from stdin
    exists <- doesFileExist file
    text <- if exists
               then readFile file
               else getContents

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
data Options = Options { filetype :: Maybe String
                       , pId :: Maybe String
                       , description :: Maybe String
                       , replies :: [String]
                       , user :: Maybe String
                       , password :: Maybe String
                       }

-- | Define options
options = [ Option "f" ["filetype"]
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
                (NoArg (const $ do hPutStrLn stderr $ usageInfo "npaste.de client v0.1\n\nUseage:" options
                                   exitWith ExitSuccess))
                "Show help"

          , Option "u" ["user"]
                (ReqArg (\arg opt -> return opt { user = Just arg }) "USERNAME")
                "Paste with your username"
          , Option "p" ["password"]
                (ReqArg (\arg opt -> return opt { password = Just arg }) "PASSWORD")
                "Use your password with your username"

          ]
