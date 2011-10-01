{-# LANGUAGE NamedFieldPuns, RankNTypes #-}

module NPaste.Database.Pastes.Info
  ( -- * Queries
    Query
  , getPasteById
  , getPasteByMD5
  , getRecentPastes
  , getPastesByUser
  , getGlobalIds
  , getPrivateIds

  , checkCustomId

    -- * Updates
  , Update
  , addPasteInfo
  , updatePasteInfo
  ) where

import Data.Maybe
import Data.ByteString (ByteString)

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils

--------------------------------------------------------------------------------
-- Queries

getPasteById :: Maybe User -> String -> Query (Maybe PasteInfo)
getPasteById mu pid = do
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_PasteInfo WHERE p_id = ? AND p_user_id = ?"
                [toSql pid, toSql (maybe (-1) u_id mu)]
  

getPasteByMD5 :: Maybe User -> ByteString -> Query (Maybe PasteInfo)
getPasteByMD5 mu hash = do
  fmap convertListToMaybe $
    querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global, p_id_is_custom \
             \  FROM Pastes WHERE p_user_id = ? AND p_md5 = ?"
             [toSql (maybe (-1) u_id mu), byteaPack hash]

getRecentPastes :: Maybe User
               -> Int          -- ^ limit
               -> Int          -- ^ offset
               -> Bool         -- ^ show hidden pastes?
               -> Query [PasteInfo]
getRecentPastes Nothing limit offset hidden =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo WHERE p_hidden = ?\
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql hidden, toSql limit, toSql offset])
getRecentPastes (Just u) limit offset hidden =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo WHERE p_hidden = ?\
                 \ WHERE p_user_id = ? \
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql hidden, toSql (u_id u), toSql limit, toSql offset])

getPastesByUser :: User
               -> Int          -- ^ limit
               -> Int          -- ^ offset
               -> Query [PasteInfo]
getPastesByUser User{u_name} limit offset =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PasteInfo \
                 \ WHERE p_user_id IN ( SELECT u_id FROM users WHERE u_name = ? ) \
                 \ ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql u_name, toSql limit, toSql offset])

--
-- IDs
--

getGlobalIds :: Query [String]
getGlobalIds =
  fmap (catMaybes . map toStr)
       (querySql "SELECT p_id FROM Pastes \
                 \ WHERE p_id_is_global = 'true'" -- include removed
                 [])
 where
  toStr [sql] = Just $ fromSql sql
  toStr _     = Nothing

getPrivateIds :: User -> Query [String]
getPrivateIds User{ u_id } =
  fmap (catMaybes . map toStr)
       (querySql "SELECT p_id FROM Pastes \
                 \ WHERE p_user_id = ? AND p_id_is_global = 'false'" -- include removed
                 [toSql u_id])
 where
  toStr [sql] = Just $ fromSql sql
  toStr _     = Nothing

checkCustomId :: User -> String -> Query Bool
checkCustomId User{ u_id } pid =
  fmap (null)
       (querySql "SELECT p_id FROM Pastes \
                 \ WHERE p_id = ? AND p_user_id = ?" -- include removed
                 [toSql pid, toSql u_id])


--------------------------------------------------------------------------------
-- Updates

addPasteInfo :: ByteString -> PasteInfo -> Update ()
addPasteInfo md5 PasteInfo{ p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global, p_id_is_custom } =
  updateSql_ "INSERT INTO Pastes (p_id, p_user_id, p_date, p_type, p_description, p_md5, p_hidden, p_id_is_global, p_id_is_custom) \
             \VALUES            (?   , ?        , ?     , ?     , ?            , ?    , ?       , ?             , ?             )"
             [ toSql p_id, toSql p_user_id, toSql p_date, toSql p_type
             , toSql p_description, byteaPack md5, toSql p_hidden, toSql p_id_is_global, toSql p_id_is_custom ]

updatePasteInfo :: PasteInfo -> Update ()
updatePasteInfo PasteInfo { p_id, p_user_id, p_type, p_description, p_hidden } =
  updateSql_ "UPDATE Pastes SET p_type = ?, p_description = ?, p_hidden = ? \
             \ WHERE p_id = ? AND p_user_id = ?"
             [ toSql p_type, toSql p_description, toSql p_hidden
             , toSql p_id, toSql p_user_id ]

