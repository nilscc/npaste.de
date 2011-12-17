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
  H.h1 $ H.a ! A.name "about" $ "About npaste.de"

  H.p $ do
    "npaste.de is back in it's 3rd version – and it's better than ever! A\
    \ completly rewritten backend makes it possible to develop new\
    \ features faster than ever before."

  H.p $ do
    "With the "
    H.a ! A.href "http://www.postgresql.org" $ "PostgreSQL"
    " database backend npaste.de is more reliable and the "
    H.a ! A.href "http://happstack.com" $ "Happstack"
    " webserver library offers a powerfull framework."

  H.p $ do
    "The "
    H.a ! A.href "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/highlighting-kate"
        $ "Kate highlighting engine"
    " offers great support for a variety of programming languages. If support\
    \ for your favorite programming language is missing, see if it is available for\
    \ download on "
    H.a ! A.href "http://kde-files.org/?xcontentmode=680"
        $ "kde-files.org"
    " or take a look at "
    H.a ! A.href "http://kate-editor.org/2005/03/24/writing-a-syntax-highlighting-file/"
        $ "how to write your own syntax highlighting file"
    ". If you want me to add your highlighting to npaste.de, please "
    H.a ! A.href "/a#contact" $ "contact me"
    "!"

  H.p $ do
    "If you're interested in the source code of npaste.de, take a look at "
    H.a ! A.href "http://github.com/mcmaniac/npaste.de" $ "github.com"
    ". If you have any questions/suggestions or would like to contribute to\
    \ npaste.de feel free to "
    H.a ! A.href "/a#contact" $ "contact me"
    "."


  H.h3 $ H.a ! A.name "howto" $ "How to use npaste.de"

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

  H.h3 $ H.a ! A.name "contact" $ "Contact"

  H.p "You can contact me via:"

  H.ul $ do
    H.li $ do "Email: "
              H.a ! A.href "mailto:mail@n-sch.de" $ "mail@npaste.de"
    H.li $    "Jabber: mcmaniac@n-sch.de"
    H.li $ do "IRC: McManiaC on "
              H.a ! A.href "irc://irc.freenode.org" $ "irc.freenode.org"

  H.h3 $ H.a ! A.name "statistics" $ "Statistics"

  H.p "There are currently:"

  H.ul $ do
    H.li $ H.toHtml $ show numP ++ " unique pastes"
    H.li $ H.toHtml $ show numU ++ " active users"
