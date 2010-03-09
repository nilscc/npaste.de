module Main where

import Happstack.Server
  ( Conf(port)
  , simpleHTTPWithSocket
  , bindPort
  -- , nullConf
  , validator
  , wdgHTMLValidator
  )

import Happstack.State
import Happstack.Util.Cron

import App.Conf
import App.Logger
import App.State
import App.Control

import Control.Concurrent
import System.Environment
import System.Log.Logger
import System.Exit
import System.Console.GetOpt 
import System.Posix.User

main :: IO ()
main = do
    let progName   = "n-sch"
        logPath    = rootPath $ defaultConf progName
        stateProxy = Proxy :: Proxy AppState
        -- user       = "nils"

    -- read arguments
    args  <- getArgs
    let eArgs = case parseConfig args of
                     (Left  e) -> Left  (unlines e)
                     (Right f) -> Right (f $ defaultConf progName)

    -- quit if error in eArgs, extract appConf otherwise
    appConf <- either (\e -> do getUserEntryForName "nils" >>= setUserID . userID
                                setupLogger logPath
                                logM progName ERROR e
                                exitFailure
                      ) (return) eArgs

    -- bind port as root
    socket <- bindPort (httpConf appConf)

    -- Switch to user, enable logging
    -- getUserEntryForName "nils" >>= setUserID . userID
    setupLogger logPath

    -- start the state system
    -- control <- startSystemState stateProxy
    control <- runTxSystem (Queue (FileSaver "_state/npaste.de")) stateProxy

    -- start the http server
    httpTid <- forkIO $ simpleHTTPWithSocket socket (httpConf appConf) (appHandler appConf)

    -- checkpoint the state once a day
    cronTid <- forkIO $ cron (60*60*24) (createCheckpoint control)

    -- wait for termination signal
    waitForTermination

    -- cleanup
    killThread httpTid
    killThread cronTid
    createCheckpoint control
    shutdownSystem control 



-- Options for getOpts
opts :: [OptDescr (AppConf -> AppConf)]
opts = [ Option ['p'] ["http-port"]   (ReqArg (\h c -> c { httpConf = (httpConf c) {port = read h} }) "port") "port to bind http server"
       , Option []    ["no-validate"] (NoArg  (\c   -> c { httpConf = (httpConf c) { validator = Nothing } })) "Turn off HTML validation"
       , Option []    ["validate"]    (NoArg  (\c   -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } })) "Turn on HTML validation"
       , Option []    ["store"]       (ReqArg (\h c -> c { store = h }) "PATH") "The directory used for database storage"
       , Option []    ["static"]      (ReqArg (\h c -> c { static = h }) "PATH") "The directory searched for static files" 
       , Option ['r'] ["root-path"]   (ReqArg (\h c -> c { rootPath = h }) "PATH") "The root directory"
       -- , Option ['d'] ["debug"]       (NoArg  (\c   -> c { debug = True })) "Debugging mode"
       , Option []    ["localhost"]   (NoArg  (\c   -> c { local = True })) "Run on localhost, ignore vHosts"
       ]

-- Parse arguments
parseConfig :: [String] -> Either [String] (AppConf -> AppConf)
parseConfig args
    = case getOpt Permute opts args of
        (flags,_,[]) -> Right $ \appConf -> foldr ($) appConf flags
        (_,_,errs)   -> Left errs
