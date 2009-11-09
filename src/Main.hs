module Main where

import Happstack.Server
  ( Conf(port)
  , simpleHTTP
  -- , nullConf
  , validator
  , wdgHTMLValidator
  )
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , Saver(Queue, FileSaver)
  -- , runTxSystem
  , shutdownSystem
  , createCheckpoint
  , startSystemState
  , waitForTermination
  )
import Happstack.Util.Cron (cron)

import App.Conf
import App.Logger   (setupLogger)
import App.State    (AppState(..))
import App.Control  (appHandler)

import Control.Concurrent   (MVar, forkIO, killThread)
import System.Environment   (getArgs)
import System.Log.Logger    (Priority(..), logM)
import System.Exit          (exitFailure)
import System.Console.GetOpt 

main = do
  let progName   = "n-sch"
      logPath    = rootPath $ defaultConf progName
      stateProxy = Proxy :: Proxy AppState
      user       = "nils"
      
  setupLogger logPath

  args <- getArgs
  appConf <- case parseConfig args of
                  (Left e) -> do logM progName ERROR (unlines e)
                                 exitFailure
                  (Right f) -> return (f $ defaultConf progName)
  
  -- start the state system
  control <- startSystemState stateProxy
  
  -- start the http server
  httpTid <- forkIO $ simpleHTTP (httpConf appConf) (appHandler appConf)

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
