{-# LANGUAGE OverloadedStrings #-}

module NPaste.Utils
  ( -- * Convenient helper functions
    convertMaybe
  , convertListToMaybe
  , convertToList
  , withJust
  , withJust_

  ) where

import Control.Monad
import Data.Convertible
import Data.Maybe

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
