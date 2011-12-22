module NPaste.Utils.Mail where

import Control.Monad.Trans
import Network.SMTP.Simple

type Name    = String
type Email   = String
type Subject = String
type Content = String

sendEMail :: MonadIO m
          => [(Maybe Name, Email)]    -- ^ to
          -> Subject
          -> Content
          -> m ()
sendEMail rcv subj cont = liftIO $
  sendSimpleMessages (const $ return ()) -- logging function
                     "127.0.2.1"
                     "npaste.de"
                     [msg]
 where
  msg = SimpleMessage
    { from    = [ NameAddr (Just "npaste.de") "noreply@npaste.de" ]
    , to      = map (uncurry NameAddr) rcv
    , subject = subj
    , body    = cont
    }
