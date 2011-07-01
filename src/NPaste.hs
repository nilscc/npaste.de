module NPaste
  ( npaste
  ) where

import NPaste.Types
import NPaste.Routes

npaste :: ServerPart Html
npaste = npasteR
