
module NPaste.Types.Html.Menu where

import NPaste.Types.Database
import NPaste.Types.Parser.Filter

data Menu = Menu
  { activeMenuSection :: ActiveMenu
  , menuStructure     :: MenuStructure
  }
  deriving (Eq, Show)

data MenuSection
  = M_Index
  | M_View          (Maybe Filter)
  | M_User          (Maybe User)
  | M_About
  deriving (Eq, Show)

newtype ActiveMenu    = ActiveMenu    { unActiveMenu    ::  MenuSection  }
  deriving (Eq, Show)

newtype MenuStructure = MenuStructure { unMenuStructure :: [MenuSection] }
  deriving (Eq, Show)
