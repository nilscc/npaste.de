module NPaste
  ( npaste
  ) where

import Happstack.Server
import NPaste.Routes

npaste :: ServerPart Response
npaste = npasteR
