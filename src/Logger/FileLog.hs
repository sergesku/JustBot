module Logger.FileLog where

import Logger.Internal

data  Config = Config
  { logPriority :: Priority
  , logFile     :: FilePath
  } deriving (Show)