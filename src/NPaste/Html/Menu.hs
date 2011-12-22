{-# LANGUAGE OverloadedStrings #-}

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
  { activeMenuSection = ActiveMenu    M_Index
  , menuStructure     = MenuStructure anonMenu
  }

-- | Menu structure if no user is logged in
anonMenu :: [MenuSection]
anonMenu =
  [ M_Index
  , M_View Nothing
  , M_User Nothing
  , M_About
  ]

userMenu :: User -> [MenuSection]
userMenu u =
  [ M_Index
  , M_View Nothing
  , M_User (Just u)
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

menuLink :: MenuSection -> AttributeValue
menuLink s = case s of
  M_View _      -> "/v"
  M_User _      -> "/u"
  M_About       -> "/a"
  _             -> "/"

subMenu :: MenuSection -> [Html]
subMenu (M_About) =
  [ H.a ! A.href "/a/howto"      $ "How to"
  , H.a ! A.href "/a/contact"    $ "Contact"
  , H.a ! A.href "/a/statistics" $ "Statistics"
  ]
subMenu (M_User Nothing) =
  [ H.a ! A.href "/u/register"      $ "Register"
  , H.a ! A.href "/u/lost-password" $ "Password lost"
  ]
subMenu (M_View (Just f)) =
  [ "Filtered by:"
  , sequence_ . intercalate [" "] $ map filterToHtml f
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
