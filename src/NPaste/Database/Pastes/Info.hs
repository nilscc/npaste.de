{-# LANGUAGE NamedFieldPuns, RankNTypes, ViewPatterns #-}

module NPaste.Database.Pastes.Info
  ( -- * Queries
    Query
  , getPasteInfoById
  , getPasteInfoByMD5
  , getRecentPasteInfos
  , getPasteInfosByUser

  , getGlobalIds
  , getPrivateIds

  , checkExistingId
  , filterExistingIds

    -- * Updates
  , Update
  , addPasteInfo
  , updatePasteInfo
  ) where

import Data.Maybe
import Data.ByteString (ByteString)

import NPaste.Database.Connection
import NPaste.Database.Users
import NPaste.Types
import NPaste.Types.Database
import NPaste.Utils

idToTuple :: ID -> (String, Int)
idToTuple (ID pid)                   = (pid,-1)
idToTuple (PrivateID User{u_id} pid) = (pid,u_id)

--------------------------------------------------------------------------------
-- Queries

getPasteInfoById :: ID -> Query (Maybe PasteInfo)
getPasteInfoById (idToTuple->(pid,pu)) = do
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_PasteInfo WHERE p_id = ? AND p_user_id = ?"
                [toSql pid, toSql pu]

getPasteInfoByMD5 :: Maybe User -> ByteString -> Query (Maybe PasteInfo)
getPasteInfoByMD5 mu hash = do
  fmap convertListToMaybe $
    querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global \
             \  FROM Pastes WHERE p_user_id = ? AND p_md5 = ?"
             [toSql (maybe (-1) u_id mu), byteaPack hash]

getRecentPasteInfos :: Maybe User
                    -> Int          -- ^ limit
                    -> Int          -- ^ offset
                    -> Bool         -- ^ show hidden pastes?
                    -> Query [PasteInfo]
getRecentPasteInfos Nothing limit offset hidden =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo WHERE p_hidden = ?\
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql hidden, toSql limit, toSql offset])
getRecentPasteInfos (Just u) limit offset hidden =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo \
                 \ WHERE p_hidden = ? AND p_user_id = ? \
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql hidden, toSql (u_id u), toSql limit, toSql offset])

getPasteInfosByUser :: User
                    -> Int          -- ^ limit
                    -> Int          -- ^ offset
                    -> Query [PasteInfo]
getPasteInfosByUser User{u_name} limit offset =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo \
                 \ WHERE p_user_id IN ( SELECT u_id FROM users WHERE u_name = ? ) \
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql u_name, toSql limit, toSql offset])

--
-- IDs
--

getGlobalIds :: Query [ID]
getGlobalIds =
  fmap (catMaybes . map toId)
       (querySql "SELECT p_id FROM pastes \
                 \ WHERE p_id_is_global = 'true'" -- include removed
                 [])
 where
  toId [sql] = Just $ ID $ fromSql sql
  toId _     = Nothing

getPrivateIds :: User -> Query [ID]
getPrivateIds User{ u_id } = do
  t <- fmap (catMaybes . map toTuple)
            (querySql "SELECT p_id, p_user_id FROM pastes \
                      \ WHERE p_user_id = ? AND p_id_is_global = 'false'" -- include removed
                      [toSql u_id])
  forM t $ \(pid,uid) -> do
    Just u <- getUserById uid
    return $ PrivateID u pid
 where
  toTuple [s_pid,s_uid] = Just $ (fromSql s_pid, fromSql s_uid)
  toTuple _             = Nothing

-- checkCustomId :: User -> String -> Query Bool
-- checkCustomId User{ u_id } pid =
--   fmap (null)
--        (querySql "SELECT p_id FROM Pastes \
--                  \ WHERE p_id = ? AND p_user_id = ?" -- include removed
--                  [toSql pid, toSql u_id])

-- | See if a ID does exist
checkExistingId :: ID -> Query Bool
checkExistingId pId =
  fmap (not . null)
       (querySql "SELECT p_id FROM pastes \
                 \ WHERE p_id = ? AND p_user_id = ?"
                 [toSql pid, toSql puserid])
 where
  (pid, puserid) = case pId of
                        ID                     i -> (i, -1)
                        PrivateID User{ u_id } i -> (i, u_id)

filterExistingIds :: [ID] -> Query [ID]
filterExistingIds = filterM checkExistingId

--------------------------------------------------------------------------------
-- Updates

addPasteInfo :: ByteString -> PasteInfo -> Update ()
addPasteInfo md5 PasteInfo{ p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global } =
  updateSql_ "INSERT INTO Pastes (p_id, p_user_id, p_date, p_type, p_description, p_md5, p_hidden, p_id_is_global) \
             \ VALUES            (?   , ?        , ?     , ?     , ?            , ?    , ?       , ?             )"
             [ toSql p_id, toSql p_user_id, toSql p_date, toSql p_type
             , toSql p_description, byteaPack md5, toSql p_hidden, toSql p_id_is_global ]

updatePasteInfo :: PasteInfo -> Update ()
updatePasteInfo PasteInfo { p_id, p_user_id, p_type, p_description, p_hidden } =
  updateSql_ "UPDATE Pastes SET p_type = ?, p_description = ?, p_hidden = ? \
             \ WHERE p_id = ? AND p_user_id = ?"
             [ toSql p_type, toSql p_description, toSql p_hidden
             , toSql p_id, toSql p_user_id ]

