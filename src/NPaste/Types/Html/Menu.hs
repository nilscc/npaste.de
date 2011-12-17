
module NPaste.Types.Html.Menu where

data MenuSection
  = M_AddNewPaste
  | M_View          (Maybe String)
  | M_About
  | M_Other
  deriving (Eq, Show)
