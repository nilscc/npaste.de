{-# LANGUAGE OverloadedStrings #-}

module NPaste.Utils where

import Control.Monad
import Data.Convertible
import Data.Maybe

convertMaybe :: Convertible a b => a ->  Maybe b
convertMaybe = either (const Nothing) Just . safeConvert

convertListToMaybe :: Convertible a b => [a] -> Maybe b
convertListToMaybe = join . fmap convertMaybe . listToMaybe


withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust m s =
  case m of
       Just a   -> s a
       Nothing  -> return ()
