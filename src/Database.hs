module Database
  ( Handle(..)
  , new
  , getConfig
  , withHandle
  ) where

import Database.Internal (Handle(..))
import Database.FileDB  (getConfig, new, withHandle)
