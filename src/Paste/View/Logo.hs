{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Logo (logoHsp) where

import HSP

logoHsp :: String -> String -> String -> HSP XML
logoHsp left right desc =
    <div id="logo">
      <p id="left"><% left %></p><p id="center">::</p><p id="right"><% right %></p>
      <p id="info"><% desc %></p>
    </div>
