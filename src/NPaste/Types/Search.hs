module NPaste.Types.Search
  ( Search (..)
  ) where

import Data.Time
import Data.ByteString
import NPaste.Types.Database

data Search
  = S_And             Search Search
  | S_Or              Search Search

    -- User searches
  | S_User            User
  | S_UserId          Int
  | S_UserName        String

    -- Paste searches
  | S_Paste           Paste
  | S_PasteId         Id
  | S_PasteType       String
  | S_PasteDesc       String
  | S_PasteCont       ByteString
  | S_PasteDate       UTCTime
  | S_PasteDateBefore UTCTime
  | S_PasteDateAfter  UTCTime
  | S_PasteHidden     Bool
  | S_PasteMd5        ByteString

    -- Tags
  | S_Tag             String
  | S_TagId           Int

    -- Replies
  | S_ReplyTo         Id
  | S_ReplyOf         Id
  deriving (Eq, Show)
