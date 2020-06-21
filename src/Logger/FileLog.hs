{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Logger.FileLog where

import           Control.Monad
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

parseConfig :: IniParser Config
parseConfig = section "Logging" $ Config
  <$> fieldDefOf "priority" (readEither . T.unpack) Debug
  <*> fieldDefOf "logFile" string "./log.log"

new :: Config -> Handle
new Config {..} = Handle $ \ pri str ->
  when (pri >= logPriority) $ appendFile logFile str