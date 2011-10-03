{-# LANGUAGE NamedFieldPuns, RankNTypes, ViewPatterns, TupleSections #-}

module NPaste.Database.Pastes
  (
    -- ** Queries
    getPasteById
  , getRecentPastes
  , getPastesByUser
  , getReplies

    -- ** Updates
  , newPaste
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.Time
import Data.Maybe
import Database.HDBC.PostgreSQL
import Happstack.Crypto.MD5 (md5)
import System.Random

import NPaste.Database.Connection
import NPaste.Database.Pastes.Info
import NPaste.Database.Pastes.Content
import NPaste.Database.Pastes.Replies
import NPaste.Database.Pastes.Tags
import NPaste.Database.Users

import qualified NPaste.Utils.Description as P
import NPaste.Types
import NPaste.Types.Database
import NPaste.Utils


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
           mpi <- getPasteInfoByMD5 mu hash
           throwError $ maybe
             (APE_Other $ show e)
             (APE_AlreadyExists mu . p_id)
             mpi
         | l == stringDataRightTruncation -> do
           throwError APE_DescTooLong
         | otherwise ->
           throwError $ APE_Other (show e)


--------------------------------------------------------------------------------
-- Queries

getPasteById :: ID -> Query (Maybe Paste)
getPasteById pid = do
  mpi <- getPasteInfoById pid
  withJust mpi $ \p_i@(PasteInfo _i ui d t de h _g) -> do
    p_id <- buildPasteId p_i
    mu   <- getUserById ui

    -- filter out invalid IDs from the description
    desc <- withJust de $ \(parseDesc->de') ->
              fmap Just . forM de' $ \descVal ->
                case descVal of
                     DescID d_id (Just d_un) -> do
                       d_mu <- getUserByName d_un
                       chk  <- withJust d_mu $ \d_u ->
                                 fmap Just . checkExistingId $ PrivateID d_u d_id
                       if chk == Just True then
                          return descVal
                        else
                          return $ DescText $ "/u/" ++ d_un ++ "/" ++ d_id ++ "/"
                     DescID d_id Nothing -> do
                       ok <- checkExistingId $ ID d_id
                       if ok then
                          return descVal
                        else
                          return $ DescText $ "/" ++ d_id ++ "/"
                     _ -> return descVal

    -- get the content and start putting everything together
    mcont <- getContent p_id
    withJust mcont $ \cont ->
      return $ Just $ Paste
        { pasteId      = p_id
        , pasteUser    = mu 
        , pasteDate    = d
        , pasteType    = t
        , pasteDesc    = desc
        , pasteContent = cont
        , pasteHidden  = h
        }

getRecentPastes :: Maybe User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Bool         -- ^ show hidden pastes?
                -> Query [Paste]
getRecentPastes mu l o h = do
  pis <- getRecentPasteInfos mu l o h
  catMaybes `fmap` mapM (\p_i -> buildPasteId p_i >>= getPasteById) pis

getPastesByUser :: User
                -> Int          -- ^ limit
                -> Int          -- ^ offset
                -> Query [Paste]
getPastesByUser u l o = do
  pis <- getPasteInfosByUser u l o
  catMaybes `fmap` mapM (\p_i -> buildPasteId p_i >>= getPasteById) pis

buildPasteId :: PasteInfo -> Query ID
buildPasteId PasteInfo{ p_id, p_user_id, p_id_is_global }
  | p_id_is_global = return $ ID p_id
  | otherwise      = do
    mu <- getUserById p_user_id
    return $ maybe ID PrivateID mu $ p_id

--------------------------------------------------------------------------------
-- Updates

-- | Adds a new Paste and eventually returns the `ID` or an `AddPasteError`
newPaste :: Maybe User               -- ^ optional user of paste
         -> Maybe String             -- ^ file type
         -> Maybe String             -- ^ description
         -> Bool                     -- ^ hidden?
         -> IdSetting
         -> ByteString               -- ^ content
         -> Update (Either AddPasteError ID)
newPaste muser mtype mdesc hide id_settings content = runErrorT $ do
    
  when (B.null content) $ throwError APE_NoContent

  let hash = B.concat . toChunks . md5 $ fromChunks [content]

  -- aquire new ID
  pid <-
    case id_settings of
         _ -> getRandomId 4 -- TODO
         -- IdRandom           -> getRandomId 4
         -- IdDefault          -> getNextId muser True
         -- IdPrivate          -> getNextId muser False
         -- IdPrivateCustom c  -> getCustom muser c

  handleSql (sqlErrorToAPE muser hash) $ do
  
    -- add Paste info
    now <- liftIO getCurrentTime
    addPasteInfo hash $ PasteInfo
      { p_id           = case pid of ID pid' -> pid'; PrivateID _ pid' -> pid'
      , p_user_id      = maybe (-1) u_id muser
      , p_date         = now
      , p_type         = mtype
      , p_description  = mdesc
      , p_hidden       = hide
      , p_id_is_global = True     -- TODO
      }
  
    -- add Paste content
    addContent pid content

    -- Add tags & replies if a description is given
    withJust_ mdesc $ \(P.parseDesc -> descVals) -> do

      -- add replies
      let rpls = P.idsOnly descVals
      replies' <- forM rpls $ \(rpid, mruname) ->
                    case mruname of
                         Nothing -> return $ ID rpid
                         Just un -> do 
                           u <- getUserByName un
                           return $ maybe (ID rpid) (PrivateID `flip` rpid) u
      replies  <- filterExistingIds replies'
      addReplies pid replies 

      -- add tags
      let tags = P.tagsOnly descVals
      addTags pid tags

    return pid


--------------------------------------------------------------------------------
-- ID generation

validChars :: [Char]
validChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

getRandomId :: Int
            -> AddPaste ID
getRandomId m = do

  ids   <- getGlobalIds
  n     <- liftIO $ randomRIO (2,m)
  iList <- rnds n (0,length validChars - 1) []

  let pid = map (validChars !!) iList
  if ID pid `elem` ids
     then getRandomId (m+1)
     else return $ ID pid

 where
  rnds :: Int -> (Int, Int) -> [Int] -> AddPaste [Int]
  rnds 0 _ akk = return akk
  rnds n r akk = do
    random' <- liftIO $ randomRIO r
    rnds (n-1) r (akk ++ [random'])

-- getCustom :: Maybe User
--           -> String
--           -> AddPaste (String, Bool, Bool)
-- getCustom Nothing _  = throwError APE_UserRequired
-- getCustom (Just u) c = do
--   available <- checkCustomId u c
--   unless available $
--     throwError (APE_InvalidCustomId c)
--   return (c, False, True)

-- getNextId :: Maybe User
--           -> Bool
--           -> AddPaste (String, Bool, Bool)
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
