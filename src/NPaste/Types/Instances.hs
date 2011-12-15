{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.Instances where

import Happstack.Server ()
import Data.Convertible
import Database.HDBC

import NPaste.Utils.Description

instance Convertible [SqlValue] Description where
  safeConvert s = fmap (parseDesc . concat) $ mapM safeConvert s

instance Convertible SqlValue Description where
  safeConvert s = fmap (parseDesc)          $      safeConvert s
