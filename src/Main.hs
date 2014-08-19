module Main where

import NPaste

import Happstack.Server
import Happstack.Util.LogFormat

import Data.Time.Format
import System.Log.Logger


main :: IO ()
main = do
  putStrLn $ "Listening on " ++ show (port npasteConf)
  simpleHTTP npasteConf npaste

--------------------------------------------------------------------------------
-- Config

npasteConf :: Conf
npasteConf = nullConf
  { port = 8090
  , logAccess = Just logMAccess'
  }

--------------------------------------------------------------------------------
-- Other

-- | Access logger (taken from Happstack.Server.Internal.Types)
--
-- TODO: Add proper logging to all of npastes routines (most import: database -- access)
logMAccess' :: FormatTime t
            => String
            -> String
            -> t
            -> String
            -> Int
            -> Integer
            -> String
            -> String
            -> IO ()
logMAccess' h user time requestLine responseCode size referer userAgent =
  logM "NPaste.AccessLog.Combined" INFO $
    formatRequestCombined h user time requestLine responseCode size referer userAgent
