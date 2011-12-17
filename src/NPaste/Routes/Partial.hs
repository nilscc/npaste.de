module NPaste.Routes.Partial
  ( partialR
  ) where

-- import Happstack.Server

-- import NPaste.Database
import NPaste.Types
-- import NPaste.State
-- import NPaste.Parser

-- import NPaste.Html.Find

partialR :: NPaste ()
partialR = do
  setNP PartialHtmlResponse
  {-
  choice
    [ dir "t" $ path $ \t -> do
        unless (validTag t) mzero
        ps       <- getPastesByTag t 20 0 False
        HtmlBody .= tagListPastes t ps
    ]
  -}
