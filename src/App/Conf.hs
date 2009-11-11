module App.Conf
    ( AppConf (..)
    , defaultConf
    )
    where

import Happstack.Server
    ( Conf (..)
    , nullConf
    )

-- | Default configuration, just adding a progname
defaultConf :: String -> AppConf
defaultConf progName = AppConf
    { httpConf  = nullConf { port = 80 }
    , store     = "_local/" ++ progName ++ "_state"
    , static    = "public"
    , rootPath  = ""
    , debug     = False
    , local     = True
    }

-- | Application configuration
data AppConf
    = AppConf { httpConf    :: Conf
              , store       :: FilePath
              , static      :: FilePath 
              , rootPath    :: FilePath
              , debug       :: Bool
              , local       :: Bool
              }
