{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Singl
import qualified Messenger            as MSG   (getConfig, withHandle)
import qualified Database             as DB    (getConfig, withHandle)
import           Types               
import           Control.Monad       
import           Core                          (getConfig, interaction)                 
import           Data.Char                     (toUpper)
import           Control.Monad.Reader          (runReaderT)
import           System.Environment            (withArgs, getArgs)
import qualified Data.Text.IO         as T     (readFile)

logOptions :: String
logOptions = strOptions "filelog, consolelog"

msgOptions :: String
msgOptions = strOptions "tg, vk"

strOptions :: String -> String
strOptions str = unlines 
  [ "Available options: " <> str
  , "Please, use one of them. Good luck ;)"
  ]
                     
main = do
    lst <- getArgs
    when (null lst) $ error $ unlines ["\nYou didn`t specify which messenger and logger to use."]
    when (length lst == 1) $ error $ unlines ["\nYou didn`t specify which logger to use."]
    let m:l:_ = lst
        chatWith' = case map toUpper m of
          "TG" -> chatWith STG
          "VK" -> chatWith SVK
          _    -> error $ unlines ["\nUnsupported messenger: " <> m, msgOptions]
    case map toUpper l of
          "FILELOG"    -> chatWith' SFile
          "CONSOLELOG" -> chatWith' SConsole
          _            -> error $ unlines ["\nUnsupported logger: " <> l, logOptions]

chatWith :: SinglMsg m -> SinglLog l -> IO ()
chatWith msg logger = do
    txt <- T.readFile "config.ini"
    let (Right config) = getConfig txt
        emsgConfig = MSG.getConfig msg txt
        dbConfig = DB.getConfig msg txt
    case emsgConfig of
      Left e -> fail e
      Right msgConfig -> MSG.withHandle msg msgConfig $ \msgH ->
                          DB.withHandle dbConfig $ \dbH -> 
                           (`runReaderT` config) (forever $ interaction msgH dbH)