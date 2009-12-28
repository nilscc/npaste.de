module Main where

import Control.Monad (when, liftM2)

import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Network.URI (escapeURIString, isAllowedInURI)

import Network.Curl.Code (CurlCode (..))
import Network.Curl.Easy (initialize, setopt, perform)
import Network.Curl.Opts (CurlOption (..))

import System (getProgName, exitWith, ExitCode(..))
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, extSeparator)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

main = do
    args <- getArgs

    -- run getOpt
    let (actions, nonOptions, error) = getOpt RequireOrder options args
    opts <- foldl' (>>=) (return $ Options Nothing Nothing) actions

    let id = pId opts
        file = case nonOptions of
                    (file:_) -> file
                    _ -> ""
        ft = case filetype opts of
                  Just f -> f
                  Nothing -> case takeExtension file of
                                  (extSeparator : ext) -> ext
                                  ext -> ext

    -- get file content/read from stdin
    exists <- doesFileExist file
    text <- if exists
               then readFile file
               else getContents

    -- post it!
    post ft id text
    return ()

-- | Post data with curl
post :: String -> Maybe String -> String -> IO CurlCode
post filetype id text = do
    curl <- initialize
    mapM_ (setopt curl) [ CurlURL "http://npaste.de"
                        , CurlPost True 
                        , CurlPostFields $ [ "content="  ++ escape text
                                           , "filetype=" ++ escape filetype
                                           , "id="       ++ escape (fromMaybe "" id)
                                           ]
                        ]
    perform curl

  where escape = escapeURIString $ isAllowedInURI &&& (not . (`elem` " !\"#$%&'()*+,-./0123456789:;<=>?@"))
        (&&&) = liftM2 (&&)

-- | Options
data Options = Options { filetype :: Maybe String
                       , pId :: Maybe String
                       }

-- | Define options
options = [ Option "f" ["filetype"]
                (ReqArg (\arg opt -> return opt { filetype = Just arg })
                        "filetype")
                "Default highlighting filetype/language"
          , Option "i" ["id"]
                (ReqArg (\arg opt -> return opt { pId = Just arg })
                        "ID")
                "Define random or custom IDs. Use 'random' or 'rand' for random ID, anything else for custom ID."
          , Option "h" ["help"]
                (NoArg (const $ do hPutStrLn stderr $ usageInfo "npaste.de client\n\nUseage:" options
                                   exitWith ExitSuccess))
                "Show help"
          ]
