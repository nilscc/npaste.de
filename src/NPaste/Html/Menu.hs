{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Menu
  ( nullMenu
  , anonMenu
  , userMenu
  , menuLink
  , menuTitle
  , subMenu
  , isCurrentMenu
  ) where

import Data.List

import Text.Blaze ((!), toValue, toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import NPaste.Types
import NPaste.Utils

nullMenu :: Menu
nullMenu = Menu
  { activeMenuSection = ActiveMenu    (Just M_Index)
  , menuStructure     = MenuStructure anonMenu
  }

-- | Menu structure if no user is logged in
anonMenu :: [MenuSection]
anonMenu =
  [ M_Index
  , M_View Nothing
  , M_HR
  , M_User Nothing
  , M_HR
  , M_About
  ]

userMenu :: User -> [MenuSection]
userMenu u =
  [ M_Index
  , M_View Nothing
  , M_HR
  , M_User (Just u)
  , M_HR
  , M_About
  ]


--------------------------------------------------------------------------------
-- Menu -> HTML conversion

menuTitle :: MenuSection -> Html
menuTitle s = case s of
  M_Index         -> "New paste"
  M_View _        -> "View pastes"
  M_User (Just _) -> "My account"
  M_User Nothing  -> "Login"
  M_About         -> "About"
  M_HR            -> H.hr

menuLink :: MenuSection -> Html -> Html
menuLink s = case s of
  M_Index       -> H.a ! A.href "/"
  M_View _      -> H.a ! A.href "/v"
  M_User _      -> H.a ! A.href "/u"
  M_About       -> H.a ! A.href "/a"
  M_HR          -> id

subMenu :: MenuSection -> [Html]
subMenu (M_About) =
  [ H.a ! A.href "/a/howto"      $ "How to"
  , H.a ! A.href "/a/contact"    $ "Contact"
  , H.a ! A.href "/a/statistics" $ "Statistics"
  ]
subMenu (M_User (Just u)) =
  [ H.span ! A.class_ "info" $ do
      "Logged in as "
      toHtml $ userName u
  , H.a ! A.href "/u/settings" $ "Settings"
  , H.a ! A.href "/u/logout"   $ "Logout"
  ]
subMenu (M_User Nothing) =
  [ H.a ! A.href "/u/register"      $ "Register"
  , H.a ! A.href "/u/lost-password" $ "Password lost"
  ]
subMenu (M_View (Just f)) =
  [ H.span ! A.class_ "info" $
      "Filtered by:"
  , H.span ! A.class_ "info" $
      sequence_ . intercalate [" "] $ map filterToHtml f
  ]
 where
  filterToHtml fval =
    let (url,text) = case fval of
                          FilterID          i -> ("/id/"   ++ i, "/"  ++ i ++ "/")
                          FilterDescription d -> ("/desc/" ++ d, "\"" ++ d ++ "\"")
                          FilterTag         t -> ("/tag/"  ++ t, "#"  ++ t)
                          FilterUsername    u -> ("/user/" ++ u, "@"  ++ u)
                          FilterLanguage    l -> let l' = maybe l id $ findLang l
                                                  in ("/lang/" ++ l', l')
     in [ H.a ! A.href (toValue $ "/v" ++ url) $ toHtml text ]
subMenu _ = []


--------------------------------------------------------------------------------
-- Helper

-- | Compare two menu sections, disregard possible context/information
isCurrentMenu :: MenuSection -> MenuSection -> Bool
isCurrentMenu (M_View _) (M_View _) = True
isCurrentMenu  m1         m2        = m1 == m2
