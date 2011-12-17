
module NPaste.Types.Html.Menu where

import NPaste.Types.Parser.Filter

data MenuSection
  = M_AddNewPaste
  | M_View          (Maybe Filter)
  | M_About
  | M_Other
  deriving (Eq, Show)
