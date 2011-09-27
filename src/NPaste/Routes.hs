module NPaste.Routes
  ( npasteR
  ) where

import Happstack.Server

import NPaste.Types
import NPaste.Routes.Index
import NPaste.Routes.Static

npasteR :: ServerPart Response
npasteR = msum
  [ nullDir >> indexR
  , staticR
  ]
