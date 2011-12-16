module NPaste.Types.Html.Context where

import NPaste.Types.Html.Menu

data HtmlContext = HtmlContext
  { title    :: Title
  , section  :: MenuSection
  , script   :: Script
  , css      :: CSS
  }
  deriving (Eq, Show)

newtype Title  = Title  { unTitle  :: Maybe String } deriving (Eq, Show)
newtype CSS    = CSS    { unCSS    :: [FilePath]   } deriving (Eq, Show)
newtype Script = Script { unScript :: [FilePath]   } deriving (Eq, Show)
