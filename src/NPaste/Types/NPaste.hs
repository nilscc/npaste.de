module NPaste.Types.NPaste where

import Control.Concurrent.MState
import Control.Monad.Error
import Happstack.Server

import NPaste.Types.Error
import NPaste.Types.State

type NPaste a = ErrorT NPasteError (MState NPasteState (ServerPartT IO)) a

type OutputM a = MState NPasteState (ServerPartT IO) a
