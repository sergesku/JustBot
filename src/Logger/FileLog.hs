{-# LANGUAGE RecordWildCards #-}

module Logger.FileLog where

import           Prelude                 hiding ( log )
import           Logger.Internal

data Config = Config
  { logPriority :: Priority
  , logFile     :: FilePath
  } deriving (Show)

new :: Config -> Handle
new Config {..} = Handle{..} where
  log priority str
    | priority >= logPriority = appendFile logFile $ mkLogMessage priority str
    | otherwise = return ()