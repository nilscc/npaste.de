
module NPaste.Types.Html.Menu where

data MenuSection
  = M_AddNewPaste
  | M_Recent
  | M_Tags
  | M_About
  | M_Other
  -- | M_Settings
  deriving (Eq, Show)
