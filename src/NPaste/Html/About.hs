{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.About
  ( aboutHtml
  ) where

import Text.Blaze (Html, (!))
import Data.Time
import System.Locale
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

aboutHtml :: Integer -> Integer -> UTCTime -> Html
aboutHtml numU numP now = do
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

  H.h3 $ H.a ! A.name "disclaimer" $ "Disclaimer"

  H.div ! A.class_ "small" $ do
    H.h4 "1. Acceptance of our Terms"

    H.p "By visiting the website npaste.de, viewing, accessing or otherwise using any of the services or information created, collected, compiled or submitted to npaste.de, you agree to be bound by the following Terms and Conditions of Service. If you do not want to be bound by our Terms your only option is not to visit, view or otherwise use the services of npaste.de. You understand, agree and acknowledge that these Terms constitute a legally binding agreement between you and npaste.de and that your use of npaste.de shall indicate your conclusive acceptance of this agreement."

    H.h4 "2. Provision of Services"

    H.p "You agree and acknowledge that npaste.de is entitled to modify, improve or discontinue any of its services at its sole discretion and without notice to you even if it may result in you being prevented from accessing any information contained in it. Furthermore, you agree and acknowledge that npaste.de is entitled to provide services to you through subsidiaries or affiliated entities."

    H.h4 "3. Proprietary Rights"

    H.p "You acknowledge and agree that npaste.de may contain proprietary and confidential information including trademarks, service marks and patents protected by intellectual property laws and international intellectual property treaties. npaste.de authorizes you to view and make a single copy of portions of its content for offline, personal, non-commercial use. Our content may not be sold, reproduced, or distributed without our written permission. Any third-party trademarks, service marks and logos are the property of their respective owners. Any further rights not specifically granted herein are reserved."

    H.h4 "4. Submitted Content"

    H.p "When you submit content to npaste.de you simultaneously grant npaste.de an irrevocable, worldwide, royalty free license to publish, display, modify, distribute and syndicate your content worldwide. You confirm and warrant that you have the required authority to grant the above license to npaste.de."

    H.h4 "5. Termination of Agreement"

    H.p "The Terms of this agreement will continue to apply in perpetuity until terminated by either party without notice at any time for any reason. Terms that are to continue in perpetuity shall be unaffected by the termination of this agreement."

    H.h4 "6. Disclaimer of Warranties"

    H.p "You understand and agree that your use of npaste.de is entirely at your own risk and that our services are provided \"As Is\" and \"As Available\". npaste.de does not make any express or implied warranties, endorsements or representations whatsoever as to the operation of the npaste.de website, information, content, materials, or products. This shall include, but not be limited to, implied warranties of merchantability and fitness for a particular purpose and non-infringement, and warranties that access to or use of the service will be uninterrupted or error-free or that defects in the service will be corrected."

    H.h4 "7. Limitation of Liability"

    H.p "You understand and agree that npaste.de and any of its subsidiaries or affiliates shall in no event be liable for any direct, indirect, incidental, consequential, or exemplary damages. This shall include, but not be limited to damages for loss of profits, business interruption, business reputation or goodwill, loss of programs or information or other intangible loss arising out of the use of or the inability to use the service, or information, or any permanent or temporary cessation of such service or access to information, or the deletion or corruption of any content or information, or the failure to store any content or information. The above limitation shall apply whether or not npaste.de has been advised of or should have been aware of the possibility of such damages. In jurisdictions where the exclusion or limitation of liability for consequential or incidental damages is not allowed the liability of npaste.de is limited to the greatest extent permitted by law."

    H.h4 "8. External Content"

    H.p "npaste.de may include hyperlinks to third-party content, advertising or websites. You acknowledge and agree that npaste.de is not responsible for and does not endorse any advertising, products or resource available from such resources or websites."

    H.h4 "9. Jurisdiction"

    H.p "You expressly understand and agree to submit to the personal and exclusive jurisdiction of the courts of the country, state, province or territory determined solely by npaste.de to resolve any legal matter arising from this agreement or related to your use of npaste.de. If the court of law having jurisdiction, rules that any provision of the agreement is invalid, then that provision will be removed from the Terms and the remaining Terms will continue to be valid."

    H.h4 "10. Entire Agreement"

    H.p "You understand and agree that the above Terms constitute the entire general agreement between you and npaste.de. You may be subject to additional Terms and conditions when you use, purchase or access other services, affiliate services or third-party content or material."

    H.h4 "11. Changes to the Terms"

    H.p "npaste.de reserves the right to modify these Terms from time to time at our sole discretion and without any notice. Changes to our Terms become effective on the date they are posted and your continued use of npaste.de after any changes to Terms will signify your agreement to be bound by them."

    H.p $ H.toHtml $ formatTime defaultTimeLocale "%B, %Y" now
