{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Logger 
  ( Handle(..)
  , Priority(..)
  , logDebug
  , logInfo
  , logError
  , logWarning
  , new
  , getConfig
  ) where

import           Logger.Internal
import qualified Logger.FileLog                as FileLog
import qualified Logger.ConsoleLog             as ConsoleLog
import           Data.Text                      ( Text )

type family Config' (l :: Log) :: *
type instance Config' FileLog = FileLog.Config
type instance Config' ConsoleLog = ConsoleLog.Config

data Singl (l :: Log) where
  SFileLog ::Singl 'FileLog
  SConsoleLog ::Singl 'ConsoleLog

data Log = FileLog | ConsoleLog deriving (Eq, Show)

new :: Singl l -> Config' l -> Handle
new SFileLog    = FileLog.new
new SConsoleLog = ConsoleLog.new

getConfig :: Singl l -> Text -> Either String (Config' l)
getConfig SFileLog    = FileLog.getConfig
getConfig SConsoleLog = ConsoleLog.getConfig
