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

  , module NPaste.Utils.Kate
  ) where

import Control.Monad
import Data.Convertible
import Data.Maybe

import NPaste.Utils.Kate

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
