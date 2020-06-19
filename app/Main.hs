{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Messenger            as MSG   (getConfig, withHandle)
import qualified Database             as DB    (getConfig, withHandle)
import           Types               
import           Control.Monad       
import           Core                          (getConfig, interaction)                 
import           Data.Char                     (toUpper)
import           Control.Monad.Reader          (runReaderT)
import           System.Environment            (withArgs, getArgs)
import qualified Data.Text.IO         as T     (readFile)

optionsStr :: String
optionsStr = unlines 
  [ "Available options: TG, VK."
  , "Please, use one of them. Good luck ;)"
  ]
                     
main = do
    lst <- getArgs
    when (null lst) $ error $ unlines ["\nYou didn`t specify which messenger to use.", optionsStr]
    let ms = head lst
    case map toUpper ms of
      "TG" -> chatWith STG
      "VK" -> chatWith SVK
      _    -> error $ unlines ["\nUnknown messenger: " <> ms, optionsStr]

chatWith :: Singl m -> IO ()
chatWith singl = do
    txt <- T.readFile "config.ini"
    let (Right config) = getConfig txt
        (emsgConfig) = MSG.getConfig singl txt
        dbConfig = DB.getConfig singl txt
    case emsgConfig of
      Left e -> fail e
      Right msgConfig -> MSG.withHandle singl msgConfig $ \msgH ->
                          DB.withHandle dbConfig $ \dbH -> 
                           (`runReaderT` config) (forever $ interaction msgH dbH)