{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module NPaste.Types.Instances where

import Happstack.Server
import Data.Aeson

instance ToMessage Value where
  toContentType _ = "application/json; charset=UTF-8"
  toMessage       = encode
