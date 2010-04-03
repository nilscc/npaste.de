{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Download
    ( showDownload
    ) where

import HSP
import Happstack.Server
import Data.List                (intercalate)

import Paste.View
import Paste.State              (defaultIds, randomIds)

-- | Show download site
showDownload :: ServerPart Response
showDownload = htmlBody [downloadHsp]


downloadHsp :: HSP XML
downloadHsp =
    <div id="main">
        <h1>Download section</h1>
        <p>The current client is available as binary for linux (i686 and x86_64, build on Arch Linux) and as
            <a href="/clientsrc/">source code</a>.</p>
        <ul>
            <li><a href="/client/x86_64/np">64bit version</a></li>
            <li><a href="/client/i686/np">32bit version</a></li>
            <li><a href="/clientsrc/">Source code</a></li>
        </ul>
        <h2>Usage:</h2>
        <pre><%     "$ np <file>                  # upload a file\n"
                ++  "$ np                         # read from stdin\n"
                ++  "$ np -f haskell <file>       # define a custom filetype (default: look for file extension or use \"plaintext\")\n"
                ++  "$ np -i <customid> <file>    # define a custom id. see below for explanation\n"
                ++  "$ np -d <description> <file> # add a description to your paste\n"
                ++  "$ np -h                      # show help\n"
        %></pre>
        <p>Currently there is a list of custom IDs which have a special behaviour:</p>
        <ul>
            <li><b>Default IDs</b> will generate a default ID:
                <pre><% intercalate ", " $ map (("\""++).(++"\"")) defaultIds %></pre>
            </li>
            <li><b>Random IDs</b> will generate a random ID for your paste and hide it from the <a href="/?view=recent">"Recent Pastes"</a> site:
                <pre><% intercalate ", " $ map (("\""++).(++"\"")) randomIds %></pre>
            </li>
        </ul>
    </div>
