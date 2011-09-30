{-# LANGUAGE NamedFieldPuns, RankNTypes, ViewPatterns, TupleSections #-}

module NPaste.Database.Posts
  (
    -- ** Queries
    getPostById
  , getRecentPosts
  , getPostsByUser
  , getContent

    -- ** Updates
  , newPost
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.Time
import Database.HDBC.PostgreSQL
import Happstack.Crypto.MD5 (md5)
import System.Random

import NPaste.Database.Connection
import NPaste.Database.Posts.Info
import NPaste.Database.Posts.Content
import NPaste.Database.Posts.Replies
import NPaste.Database.Posts.Tags
import NPaste.Database.Users

import qualified NPaste.Tools.Description as P
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Errors

-- | Convert SQL exceptions to APE errors
sqlErrorToAPE :: Maybe User
              -> ByteString     -- ^ MD5 hash
              -> SqlError
              -> AddPost a
sqlErrorToAPE mu hash e =
  case seState e of
       l | l == uniqueViolation -> do
           mpi <- getPostByMD5 mu hash
           throwError $ maybe
             (APE_Other $ show e)
             (APE_AlreadyExists mu . p_id)
             mpi
         | l == stringDataRightTruncation -> do
           throwError APE_DescTooLong
         | otherwise ->
           throwError $ APE_Other (show e)



--------------------------------------------------------------------------------
-- Add a new post

-- | Adds a new post and eventually returns the `ID` or an `AddPostError`
newPost :: Maybe User               -- ^ optional user of paste
        -> Maybe String             -- ^ file type
        -> Maybe String             -- ^ description
        -> Bool                     -- ^ hidden?
        -> IdSetting
        -> ByteString               -- ^ content
        -> Update (Either AddPostError ID)
newPost muser mtype mdesc hide id_settings content = runErrorT $ do
    
  when (B.null content) $ throwError APE_NoContent

  let hash = B.concat . toChunks . md5 $ fromChunks [content]

  -- aquire new ID
  (pid, pid_is_global, pid_is_custom) <-
    case id_settings of
         IdRandom           -> getRandomId 4
         -- IdDefault          -> getNextId muser True
         -- IdPrivate          -> getNextId muser False
         IdPrivateCustom c  -> getCustom muser c

  handleSql (sqlErrorToAPE muser hash) $ do
  
    -- add post info
    now <- liftIO getCurrentTime
    addPostInfo hash $ PostInfo
      { p_id           = pid
      , p_user_id      = maybe (-1) u_id muser
      , p_date         = now
      , p_type         = mtype
      , p_description  = mdesc
      , p_hidden       = hide
      , p_id_is_global = pid_is_global
      , p_id_is_custom = pid_is_custom
      }
  
    -- add post content
    addContent muser pid content

    -- Add tags & replies if a description is given
    withJust mdesc $ \(P.parseDesc -> descVals) -> do

      -- add replies
      let rpls = P.idsOnly descVals
      rplsWithUsers <- forM rpls $ \(rpid, mruname) ->
        fmap (, rpid) (maybe (return Nothing) getUserByName mruname)
      addReplies muser pid rplsWithUsers

      -- add tags
      let tags = P.tagsOnly descVals
      addTags muser pid tags

    return $
      case muser of
           Just u | pid_is_global -> PrivateID u pid
           _                      -> ID pid


--------------------------------------------------------------------------------
-- ID generation

validChars :: [Char]
validChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

getRandomId :: Int
            -> AddPost (String, Bool, Bool)
getRandomId m = do

  ids   <- getGlobalIds
  n     <- liftIO $ randomRIO (2,m)
  iList <- rnds n (0,length validChars - 1) []

  let pid = map (validChars !!) iList
  if pid `elem` ids
     then getRandomId (m+1)
     else return (pid, True, False)

 where
  rnds :: Int -> (Int, Int) -> [Int] -> AddPost [Int]
  rnds 0 _ akk = return akk
  rnds n r akk = do
    random' <- liftIO $ randomRIO r
    rnds (n-1) r (akk ++ [random'])

getCustom :: Maybe User
          -> String
          -> AddPost (String, Bool, Bool)
getCustom Nothing _  = throwError APE_UserRequired
getCustom (Just u) c = do
  available <- checkCustomId u c
  unless available $
    throwError (APE_InvalidCustomId c)
  return (c, False, True)

-- getNextId :: Maybe User
--           -> Bool
--           -> AddPost (String, Bool, Bool)
-- getNextId mUser global = do
--   let global' = global || isNothing mUser
--   ids <- if global'
--              then getGlobalIds
--              else getPrivateIds (fromJust mUser)
--   let pid = head $
--         dropWhile (\pid' -> pid' `elem` reservedIds || pid' `elem` ids)
--                   everything
--   return (pid, global', False)
--  where
--   everything  = concat $ iterate func chars
--   func list   = concatMap (\char -> map (char ++) list) chars
--   chars       = map (:[]) validChars
