module App.Logger (setupLogger) where

import System.Log.Logger
    ( Priority(..)
    , rootLoggerName
    , setLevel
    , setHandlers
    , updateGlobalLogger
    )
import System.Log.Handler.Simple
    ( fileHandler
    , streamHandler
    )
import System.IO
    ( stdout
    )

setupLogger :: String -> IO ()
setupLogger _ = do

    appLog    <- fileHandler "app.log" INFO
    accessLog <- fileHandler "access.log" INFO
    stdoutLog <- streamHandler stdout NOTICE

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel DEBUG . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel INFO . setHandlers [accessLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel NOTICE . setHandlers [stdoutLog])
