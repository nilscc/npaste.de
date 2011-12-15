{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Users.Password
  ( packPassword
  , checkPassword
  , changePassword
  ) where

import Control.Monad
import Control.Monad.Trans
import Codec.Utils
import Data.ByteString (ByteString, unpack, pack)
import Data.ByteString.Internal (c2w)
import Data.Digest.SHA512
import System.Random
import Numeric

import NPaste.Database.Connection
import NPaste.Types



--------------------------------------------------------------------------------
-- Password validation

getPassword :: User -> Query ByteString
getPassword User{ u_id } = do
  [[s]] <- querySql "SELECT password FROM users WHERE id = ?"
                    [toSql u_id]
  return $ byteaUnpack s

-- | Check if the provided password of a user is correct.
checkPassword :: User
              -> String         -- ^ password (plaintext)
              -> Query Bool
checkPassword u pw = do
  bs <- getPassword u
  return . checkSaltedHash pw $ unpack bs

-- | Change the password of a user. Returns `True` on success, `False`
-- otherwise.
changePassword :: User
               -> String        -- ^ new password (plaintext)
               -> Update Bool
changePassword User{ u_id } str = do
  mpw <- packPassword str
  case mpw of
       Nothing -> return False
       Just pw -> do
         updateSql_ "UPDATE users SET password = ? \
                    \ WHERE id = ?"
                    [ pw, toSql u_id ]
         return True

-------------------------------------------------------------------------------
-- Password generation

packPassword :: MonadIO m => String -> m (Maybe SqlValue)
packPassword str = do
  liftM (fmap $ byteaPack . pack) $
        mkSaltedHash str

type SaltedHash = [Octet]

mkSaltedHash :: MonadIO m
             => String
             -> m (Maybe SaltedHash)
mkSaltedHash str
  | null str = return Nothing
  | otherwise = do
    salt <- liftIO randomSalt
    let salt' = strToOctets salt
        str' = strToOctets str
        h = slowHash (salt' ++ str')
    return . Just $ salt' ++ h

checkSaltedHash :: String -> SaltedHash -> Bool
checkSaltedHash str h =
  h == salt ++ (slowHash $ salt ++ (strToOctets str))
 where
  salt = take saltLength h

saltLength :: Num t => t
saltLength = 16

strToOctets :: String -> [Octet]
strToOctets = listToOctets . map c2w

slowHash :: [Octet] -> [Octet]
slowHash a = (iterate hash a) !! 512

randomSalt :: IO String
randomSalt =
  liftM concat $ sequence $ take saltLength $ repeat $
    randomRIO (0 :: Int, 15) >>= return . flip showHex ""
