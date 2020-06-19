{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.ConsoleLog where

import           Control.Monad                (when)
import           Logger.Internal
import           Text.Read                    (readEither)
import           Data.Ini.Config
import           Prelude               hiding ( log )
import           Data.Text                    (Text)
import qualified Data.Text          as T

data Config = Config 
  { logPriority :: Priority }
  deriving (Show)

getConfig :: Text -> Either String Config
getConfig = (`parseIniFile` parseConfig)

parseConfig = section "Logging" $ Config 
  <$> fieldDefOf "logPriority" (readEither . T.unpack) Debug

new :: Config -> Handle
new Config {..} = Handle $ \ pri str -> do
  let io = putStrLn $ mkLogMessage pri str
  when (pri >= logPriority) io