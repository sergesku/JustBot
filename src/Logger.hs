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

import           Data.Singl
import           Data.Ini.Config
import           Logger.Internal
import qualified Logger.FileLog                as FileLog
import qualified Logger.ConsoleLog             as ConsoleLog
import           Data.Text                      ( Text )

type family Config' (l :: Log) :: *
type instance Config' FileLog = FileLog.Config
type instance Config' ConsoleLog = ConsoleLog.Config

new :: SinglLog l -> Config' l -> Handle
new SFile    = FileLog.new
new SConsole = ConsoleLog.new

getConfig :: SinglLog l -> Text -> Either String (Config' l)
getConfig SFile = (`parseIniFile` FileLog.parseConfig)
getConfig SConsole = (`parseIniFile` ConsoleLog.parseConfig)