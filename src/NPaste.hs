module NPaste
  ( npaste
  ) where

import Happstack.Server
import NPaste.State
import NPaste.Routes
import NPaste.Types

npaste :: ServerPart Response
npaste = do
  ((), newstate) <- runNPaste npasteNullState npasteR
  compose newstate

compose :: NPasteState -> ServerPart Response
compose st = do
  let code = unResponseCode $ responseCode st
  case responseFormat st of
       HtmlResponse -> do
         let ctxt  =               htmlContext st
             html  =               htmlBody    st
             frame = unHtmlFrame $ htmlFrame   st
         sequence_ $ runBeforeResponse st
         code . toResponse $ frame ctxt html
       PartialHtmlResponse ->
         code . toResponse . unHtmlBody $ htmlBody st
       PlainResponse rq rsp ->
         localRq (const rq) rsp
