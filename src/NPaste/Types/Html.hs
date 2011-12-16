module NPaste.Types.Html
  ( -- * Main HTML framework
    module NPaste.Types.Html.Context
  , module NPaste.Types.Html.Menu

    -- ** Websites
  , Html
  , AttributeValue
  , module NPaste.Types.Html.Index

    -- ** Type classes
  , module NPaste.Types.Html.Valued
  ) where

import Text.Blaze

import NPaste.Types.Html.Context
import NPaste.Types.Html.Menu
import NPaste.Types.Html.Index
import NPaste.Types.Html.Valued
