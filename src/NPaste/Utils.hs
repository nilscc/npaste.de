{-# LANGUAGE OverloadedStrings #-}

module NPaste.Utils
  ( -- * Convenient helper functions
    readMaybe
  , convertMaybe
  , convertListToMaybe
  , convertToList
  , withJust
  , withJust_
  , require

    -- ** Happstack helper functions
  , module NPaste.Utils.Happstack

    -- ** State functions
  , module NPaste.Utils.State

    -- ** Database functions
  , module NPaste.Utils.Database

    -- ** Other
  , module NPaste.Utils.Kate
  , module NPaste.Utils.Mail
  ) where

import Control.Monad
import Data.Convertible
import Data.Maybe

import NPaste.Utils.Database
import NPaste.Utils.Kate
import NPaste.Utils.Mail
import NPaste.Utils.State
import NPaste.Utils.Happstack

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _         -> Nothing

convertMaybe :: Convertible a b => a ->  Maybe b
convertMaybe = either (const Nothing) Just . safeConvert

convertListToMaybe :: Convertible a b => [a] -> Maybe b
convertListToMaybe = join . fmap convertMaybe . listToMaybe

-- | Try to convert all elements, if not possible the element is lost!
convertToList :: Convertible a b => [a] -> [b]
convertToList = catMaybes . map convertMaybe

withJust :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
withJust m s =
  case m of
       Just a   -> s a
       Nothing  -> return Nothing

withJust_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust_ m s =
  case m of
       Just a   -> s a
       Nothing  -> return ()

require :: MonadPlus m => m (Maybe a) -> m a
require np = np >>= maybe mzero return
