module NPaste.Types.Html.Body where

import Text.Blaze
import NPaste.Types.Database.User
import NPaste.Types.Html.Menu

data HtmlBody = HtmlBody
  { title    :: Maybe String
  , section  :: MenuSection
  , user     :: Maybe User
  , script   :: [FilePath]
  , css      :: [FilePath]
  , html     :: Html
  }
