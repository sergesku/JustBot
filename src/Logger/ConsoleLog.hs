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

parseConfig = section "Logging" $ Config 
  <$> fieldDefOf "logPriority" (readEither . T.unpack) Debug

new :: Config -> Handle
new Config {..} = Handle $ \ pri str ->
  when (pri >= logPriority) $ putStrLn str