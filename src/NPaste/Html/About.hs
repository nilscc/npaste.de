{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.About
  ( aboutHtml
  ) where

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

aboutHtml :: Integer -> Integer -> Html
aboutHtml numU numP = do
  H.h1 "About npaste.de"

  H.h3 "How to use npaste.de"

  H.p $ do
    "To create a paste simply go to "
    H.a ! A.href "/" $ "the start page"
    " and enter your paste, add an optional description (see below) and select \
    \your highlighting language of choice."

  H.p $ do
    "The "
    H.em "description"
    " has a special markup language. You can: "

  H.ul $ do
    H.li $ do
      "Reply to (mention) a paste by adding "
      H.pre ! A.class_ "inline" $ "/<ID>/"
      " to your description. This is done automatically if you click the "
      H.pre ! A.class_ "inline" $ "New reply"
      " button on the viewing page of a paste."
    H.li $ do
      "Use tags to make it easier to find your pastes again. Simply add "
      H.pre ! A.class_ "inline" $ "#<TAG>"
      " to your description. A tag may consist of any number or char. Most \
      \ of the common special chars are also supported."


  H.p $ "To customize the presentation of your paste you can use the URL to change \
        \syntax highlighting or the HTML layout:"

  H.ul $ do
    H.li $ do
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>/"
      " shows the full paste with layout and highlighting (notice the trailing slash)"
    H.li $ do
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>/<LANGUAGE>"
      " shows the full paste with the new "
      H.pre ! A.class_ "inline" $ "<LANGUAGE>"
      " syntax highlighting. You can either define the full language name, the common\
      \ file extension or a full file name with extension, e.g. "
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>/Haskell"
      ", "
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>/hs"
      " or "
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>/example.hs"
    H.li $ do
      H.pre ! A.class_ "inline" $ "npaste.de/<ID>"
      " without a trailing slash shows the paste as plaintext without HTML markup \
      \(which makes it easy to download a paste)"

  H.p $ "On the main website all pages are accessible via one letter keywords:"

  H.ul $ do
    H.li $ do H.pre ! A.class_ "inline" $ "npaste.de"
              " or "
              H.pre ! A.class_ "inline" $ "npaste.de/"
              " is where you add new pastes"
    H.li $ do H.pre ! A.class_ "inline" $ "npaste.de/r"
              " lists all "
              H.em "recent"
              " pastes"
    H.li $ do H.pre ! A.class_ "inline" $ "npaste.de/a"
              " is the current "
              H.em "about"
              " page"
    H.li $ do H.pre ! A.class_ "inline" $ "npaste.de/t/<TAG>"
              " lists all pastes with the "
              H.em "tag"
              " "
              H.pre ! A.class_ "inline" $ "#<TAG>"

  H.h3 "Statistics"

  H.p "There are currently:"

  H.ul $ do
    H.li $ H.toHtml $ show numP ++ " unique pastes"
    H.li $ H.toHtml $ show numU ++ " active users"
