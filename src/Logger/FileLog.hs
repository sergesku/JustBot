{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Logger.FileLog where

import           Text.Read                      (readEither)
import qualified Data.Text               as T
import           Data.Text                      (Text)
import           Data.Ini.Config
import           Prelude                 hiding ( log )
import           Logger.Internal

data Config = Config
  { logPriority :: Priority
  , logFile     :: FilePath
  } deriving (Show)

getConfig :: Text -> Either String Config
getConfig = (`parseIniFile` parseConfig)

parseConfig :: IniParser Config
parseConfig = section "Logging" $ Config
  <$> fieldDefOf "priority" (readEither . T.unpack) Debug
  <*> fieldDefOf "logFile" string "./log.log"

new :: Config -> Handle
new Config {..} = Handle{..} where
  log priority str
    | priority >= logPriority = appendFile logFile $ mkLogMessage priority str
    | otherwise = return ()