module Main where

import NPaste

import Happstack.Server
import Happstack.Util.LogFormat

import Data.Time.Format
import System.Log.Logger


main :: IO ()
main = simpleHTTP npasteConf npaste

--------------------------------------------------------------------------------
-- Config

npasteConf :: Conf
npasteConf = nullConf
  { port = 8080
  , logAccess = Just logMAccess
  }

--------------------------------------------------------------------------------
-- Other

-- | Access logger (taken from Happstack.Server.Internal.Types)
logMAccess :: FormatTime t
           => String
           -> String
           -> t
           -> String
           -> Int
           -> Integer
           -> String
           -> String
           -> IO ()
logMAccess host user time requestLine responseCode size referer userAgent =
  logM "NPaste.AccessLog.Combined" INFO $
    formatRequestCombined host user time requestLine responseCode size referer userAgent
