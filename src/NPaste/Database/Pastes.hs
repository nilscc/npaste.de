{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module NPaste.Database.Pastes
  ( -- ** Queries
    getPasteById
  , getRecentPastes
  , getPastesByUser
  , getReplies

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

import Happstack.Crypto.MD5 (md5)

import NPaste.Database.Pastes.Replies
import NPaste.Database.Pastes.Tags
import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils
import qualified NPaste.Utils.Description as P

getPasteById :: Id -> Query (Maybe Paste)
getPasteById pid =
  fmap convertListToMaybe $
    querySql "SELECT id, user_id, date, type, description, content, hidden \
             \  FROM pastes                                                \
             \ WHERE id = ?"
             [ toSql pid ]

getPasteByMD5 :: Maybe User -> ByteString -> Query (Maybe Paste)
getPasteByMD5 mu hash =
  fmap convertListToMaybe $
    querySql "SELECT id, user_id, date, type, description, content, hidden \
             \  FROM pastes WHERE user_id = ? AND md5 = ?"
             [ toSql (maybe (-1) u_id mu), byteaPack hash ]

getRecentPastes :: Maybe User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Bool         -- ^ show hidden pastes?
                -> Query [Paste]
getRecentPastes mu limit offset hidden =
  fmap convertToList $
    case mu of
         Just u ->
           querySql "SELECT id, user_id, date, type, description, content, hidden \
                    \  FROM pastes WHERE hidden IN (?,FALSE) AND user_id = ?      \
                    \ ORDER BY date DESC LIMIT ? OFFSET ?"
                    [ toSql hidden, toSql (u_id u), toSql limit, toSql offset ]
         Nothing ->
           querySql "SELECT id, user_id, date, type, description, content, hidden \
                    \  FROM pastes WHERE hidden IN (?,FALSE)                      \
                    \ ORDER BY date DESC LIMIT ? OFFSET ?"
                    [ toSql hidden, toSql limit, toSql offset ]

getPastesByUser :: User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Query [Paste]
getPastesByUser User{ u_id } limit offset =
  fmap convertToList $
    querySql "SELECT id, user_id, date, type, description, content, hidden \
             \  FROM pastes WHERE user_id = ?                              \
             \ ORDER BY date DESC LIMIT ? OFFSET ?"
             [ toSql u_id, toSql limit, toSql offset ]

getReplies :: Id -> Query [Paste]
getReplies pid =
  fmap convertToList $
    querySql "SELECT id, user_id, date, type, description, content, hidden \
             \  FROM pastes                                                \
             \ WHERE id IN (SELECT reply_id FROM replies WHERE paste_id = ?) "
             [ toSql pid ]

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

    let uid   = maybe (-1) u_id muser
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
