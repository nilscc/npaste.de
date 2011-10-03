{-# LANGUAGE OverloadedStrings #-}

module NPaste.Utils
  ( -- * Convenient helper functions
    convertMaybe
  , convertListToMaybe
  , withJust

    -- * Description
  , module NPaste.Utils.Description
  ) where

import Control.Monad
import Data.Convertible
import Data.Maybe

import NPaste.Utils.Description

convertMaybe :: Convertible a b => a ->  Maybe b
convertMaybe = either (const Nothing) Just . safeConvert

convertListToMaybe :: Convertible a b => [a] -> Maybe b
convertListToMaybe = join . fmap convertMaybe . listToMaybe


withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust m s =
  case m of
       Just a   -> s a
       Nothing  -> return ()
