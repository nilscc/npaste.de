module NPaste.Routes
  ( npasteR
  ) where

import Happstack.Server

import NPaste.Types
import NPaste.Routes.Index

npasteR :: ServerPart Html
npasteR = msum
  [ do nullDir
       indexR
  ]
