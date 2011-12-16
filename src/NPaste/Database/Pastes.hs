{-# LANGUAGE NamedFieldPuns, ViewPatterns, FlexibleContexts,
   MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Database.Pastes
  ( -- ** Queries
    getPasteById
  , getRecentPastes
  , getPastesByUser
  , getReplies
  , getPastesByTag
  , findPastes

    -- ** Updates
  , newPaste
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromChunks, toChunks)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Time
import Database.HDBC.PostgreSQL
import System.Random
import qualified Data.Set as S

import Happstack.Crypto.MD5 (md5)

import NPaste.Database.Pastes.Replies
import NPaste.Database.Pastes.Tags
import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils
import qualified NPaste.Utils.Description as P


--------------------------------------------------------------------------------
-- Queries

instance Select [Paste] where
  select         = withSelectStr "SELECT p.id, p.user_id, p.date, p.type, p.description, p.content, p.hidden FROM pastes p"
  convertFromSql = convertToList

instance Select (Maybe Paste) where
  select         = withSelectStr "SELECT p.id, p.user_id, p.date, p.type, p.description, p.content, p.hidden FROM pastes p"
  convertFromSql = convertListToMaybe


getPasteById :: Id -> Query (Maybe Paste)
getPasteById pid = findPastes 1 0 $ S_PasteId pid

getPasteByMD5 :: Maybe User -> ByteString -> Query (Maybe Paste)
getPasteByMD5 mu hash =
  findPastes 1 0 $ S_And (S_UserId (maybe (-1) userId mu))
                         (S_PasteMd5 hash)

getPastesByTag :: String        -- ^ Tag
               -> Int           -- ^ limit
               -> Int           -- ^ offset
               -> Bool          -- ^ show hidden pastes?
               -> Query [Paste]
getPastesByTag tag limit offset hidden =
  findPastes limit offset $
    S_And (S_PasteHidden hidden)
          (S_Tag tag)

getRecentPastes :: Maybe User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Bool         -- ^ show hidden pastes?
                -> Query [Paste]
getRecentPastes mu limit offset hidden =
  findPastes limit offset $
    case mu of
         Just u  -> S_And (S_PasteHidden hidden) (S_User u)
         Nothing -> S_PasteHidden hidden

getPastesByUser :: User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Query [Paste]
getPastesByUser u limit offset =
  findPastes limit offset $ S_User u

getReplies :: Id
           -> Int             -- ^ limit
           -> Int             -- ^ offset
           -> Query [Paste]
getReplies pid limit offset =
  findPastes limit offset $ S_ReplyTo pid

findPastes :: Select res
           => Int         -- ^ Limit
           -> Int         -- ^ Offset
           -> Search
           -> Query res
findPastes limit offset crits =
  select (joins ++ " WHERE " ++ toWhere crits ++ " ORDER BY date DESC LIMIT ? OFFSET ?")
         (toSql' crits ++ [toSql limit, toSql offset])
 where
  joins                         = unwords . S.toList $ joins' crits
  joins'  (S_And s1 s2)         = joins' s1 `S.union` joins' s2
  joins'  (S_Or  s1 s2)         = joins' s1 `S.union` joins' s2
  joins'  (S_UserName _)        = S.singleton "JOIN users u   ON u.id = p.user_id"
  joins'  (S_Tag _)             = S.singleton "JOIN tags t    ON t.id = p.id"
  joins'  (S_TagId _)           = S.singleton "JOIN tags t    ON t.id = p.id"
  joins'  (S_ReplyOf _)         = S.singleton "JOIN replies r ON r.paste_id = p.id"
  joins'  (S_ReplyTo _)         = S.singleton "JOIN replies r ON r.reply_id = p.id"
  joins'  _                     = S.empty

  toWhere (S_And s1 s2)         = "(" ++ toWhere s1 ++ " AND " ++ toWhere s2 ++ ")"
  toWhere (S_Or  s1 s2)         = "(" ++ toWhere s1 ++ " OR  " ++ toWhere s2 ++ ")"
  toWhere (S_User _)            = "p.user_id = ?"
  toWhere (S_UserId _)          = "p.user_id = ?"
  toWhere (S_UserName _)        = "u.name = ?"
  toWhere (S_Paste _)           = "p.id = ?"
  toWhere (S_PasteId _)         = "p.id = ?"
  toWhere (S_PasteType _)       = "p.type = ?"
  toWhere (S_PasteDesc _)       = "p.description = ?"
  toWhere (S_PasteCont _)       = "to_tsvector(p.content) @@ to_tsquery(?)" -- TODO
  toWhere (S_PasteMd5 _)        = "p.md5 = ?"
  toWhere (S_PasteDate _)       = "p.date = ?"
  toWhere (S_PasteDateBefore _) = "p.date < ?"
  toWhere (S_PasteDateAfter _)  = "p.date > ?"
  toWhere (S_PasteHidden _)     = "p.hidden IN (?,FALSE)"
  toWhere (S_Tag _)             = "t.tag = ?"
  toWhere (S_TagId _)           = "t.id = ?"
  toWhere (S_ReplyOf _)         = "r.paste_id = ?"
  toWhere (S_ReplyTo _)         = "r.reply_id = ?"

  toSql'  (S_And s1 s2)         = toSql' s1 ++ toSql' s2
  toSql'  (S_Or  s1 s2)         = toSql' s1 ++ toSql' s2
  toSql'  (S_User u)            = [toSql $ userId u]
  toSql'  (S_UserId i)          = [toSql i]
  toSql'  (S_UserName n)        = [toSql n]
  toSql'  (S_Paste p)           = [toSql $ pasteId p]
  toSql'  (S_PasteId i)         = [toSql i]
  toSql'  (S_PasteType t)       = [toSql t]
  toSql'  (S_PasteDesc d)       = [toSql d]
  toSql'  (S_PasteCont c)       = [toSql c]
  toSql'  (S_PasteMd5 m)        = [byteaPack m]
  toSql'  (S_PasteDate d)       = [toSql d]
  toSql'  (S_PasteDateBefore d) = [toSql d]
  toSql'  (S_PasteDateAfter d)  = [toSql d]
  toSql'  (S_PasteHidden h)     = [toSql h]
  toSql'  (S_Tag t)             = [toSql t]
  toSql'  (S_TagId i)           = [toSql i]
  toSql'  (S_ReplyOf i)         = [toSql i]
  toSql'  (S_ReplyTo i)         = [toSql i]




--------------------------------------------------------------------------------
-- Updates

newPaste :: Maybe User
         -> Maybe String          -- ^ type
         -> Maybe String          -- ^ description
         -> Bool                  -- ^ hidden?
         -> ByteString            -- ^ content
         -> Update (Either AddPasteError Id)
newPaste muser mtype mdesc hidden cont = runErrorT $ do

  when (B.null cont) $ throwError APE_NoContent

  let hash = B.concat . toChunks . md5 $ fromChunks [cont]

  pid <- getRandomId 4

  handleSql (sqlErrorToAPE muser hash) $ do
    now <- liftIO getCurrentTime

    let uid   = maybe (-1) userId muser
    updateSql_ "INSERT INTO pastes (id, user_id, date, type, description, content, md5, hidden) \
               \     VALUES        (?,  ?,       ?,    ?,    ?,           ?,       ?,   ?     ) "
               [ toSql pid, toSql uid, toSql now, toSql mtype, toSql mdesc
               , byteaPack cont, byteaPack hash, toSql hidden ]

    -- Add tags & replies if a description is given
    withJust_ mdesc $ \(P.parseDesc -> descVals) -> do

      -- add replies
      let replies = P.idsOnly descVals
      replies'  <- filterExistingIds replies
      addReplyIds pid replies'

      -- add tags
      let tags = P.tagsOnly descVals
      addTags pid tags

    return pid



--------------------------------------------------------------------------------
-- Ids

validChars :: [Char]
validChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

getRandomId :: Int
            -> AddPaste Id
getRandomId m = do

  ids   <- getGlobalIds
  n     <- liftIO $ randomRIO (2,m)
  iList <- rnds n (0,length validChars - 1) []

  let pid = map (validChars !!) iList
  if pid `elem` ids
     then getRandomId (m+1)
     else return pid

 where
  rnds :: Int -> (Int, Int) -> [Int] -> AddPaste [Int]
  rnds 0 _ akk = return akk
  rnds n r akk = do
    random' <- liftIO $ randomRIO r
    rnds (n-1) r (akk ++ [random'])

getGlobalIds :: Query [Id]
getGlobalIds =
  fmap convertToList (querySql "SELECT id FROM pastes" [])

checkExistingId :: Id -> Query Bool
checkExistingId pid =
  fmap ((==) [[SqlInteger 1]]) $
       querySql "SELECT count(*) FROM pastes WHERE id = ?" [ toSql pid ]

filterExistingIds :: [Id] -> Query [Id]
filterExistingIds = filterM checkExistingId

--------------------------------------------------------------------------------
-- Errors

-- | Convert SQL exceptions to APE errors
sqlErrorToAPE :: Maybe User
              -> ByteString     -- ^ MD5 hash
              -> SqlError
              -> AddPaste a
sqlErrorToAPE mu hash e =
  case seState e of
       l | l == uniqueViolation -> do
           mpi <- getPasteByMD5 mu hash
           throwError $ maybe
             (APE_Other $ show e)
             APE_AlreadyExists
             mpi
         | l == stringDataRightTruncation -> do
           throwError APE_DescTooLong
         | otherwise ->
           throwError $ APE_Other (show e)
