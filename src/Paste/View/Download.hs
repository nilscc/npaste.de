{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Download
    ( showDownload
    ) where

import HSP
import Happstack.Server

import Paste.View               (xmlResponse, htmlBody, getLogin)

-- | Show download site
showDownload :: ServerPart Response
showDownload = do
    login <- getLogin
    xmlResponse $ htmlBody login [downloadHsp]


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
        <p>Useage:</p>
        <pre><%     "$ np <file>                  # upload a file\n"
                ++  "$ np                         # read from stdin\n"
                ++  "$ np -f haskell <file>       # define a custom filetype (default: look for file extension or use \"plaintext\"\n"
                ++  "$ np -i <customid> <file>    # define a custom id\n"
                ++  "$ np -d <description> <file> # add a description to your paste\n"
                ++  "$ np -h                      # show help\n"
        %></pre>
        <p>Of course it is also possible to combine these options (filetype, id and description).</p>
    </div>
