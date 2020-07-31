{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Error
import           Control.Exception
import           System.Console.GetOpt
import           Data.Singl
import qualified Logger               as Log   (getConfig, new, withHandle)
import qualified Messenger            as MSG   (getConfig, withHandle)
import qualified Database             as DB    (getConfig, withHandle)
import           Data.Update               
import           Control.Monad       
import           Core                          (getConfig, interaction)                 
import           Data.Char                     (toUpper, isSpace)
import           System.Environment            (withArgs, getArgs)
import qualified Data.Text.IO         as T     (readFile)

data AppOption where
  MsgOption :: SinglMsg m -> AppOption
  LogOption :: SinglLog l -> AppOption

options :: [OptDescr (Either String AppOption)]
options = [ Option ['m'] ["messenger"] (ReqArg readMsgOption "Messenger") "Messenger to run the bot :: tg | vk"
          , Option ['l'] ["log"] (ReqArg readLogOption "Log") "Log output :: console | file"
          ]

processStr :: String -> String
processStr = map toUpper . trim
  where trim = f . f
        f    = reverse . dropWhile isSpace

readMsgOption :: String -> Either String AppOption
readMsgOption str = case processStr str of
  "TG" -> Right $ MsgOption STG
  "VK" -> Right $ MsgOption SVK
  _    -> Left  $ "Unsapported Messenger type: " <> str <> "."

readLogOption :: String -> Either String AppOption
readLogOption str = case processStr str of
  "FILE"    -> Right $ LogOption SFile
  "CONSOLE" -> Right $ LogOption SConsole
  _         -> Left  $ "Unsapported Log type: " <> str <> "."

isMsgOption :: AppOption -> Bool
isMsgOption (MsgOption _ ) = True
isMsgOption _              = False

isLogOption :: AppOption -> Bool
isLogOption (LogOption _) = True
isLogOption _             = False

getOption :: String -> (AppOption -> Bool) -> [AppOption] -> Either String AppOption
getOption opt f lst = case filter f lst of 
  [x] -> Right x
  []  -> Left $ "You didn`t specify which " <> opt <> " to use."
  _   -> Left $ "Too many " <> opt <> " options specified."

extractOptions :: [String] -> Either String (AppOption, AppOption)
extractOptions args = do
  let (eOpts, nonOpts, unrec) = getOpt RequireOrder options args
  unless (null nonOpts) $ Left $ unlines $ map ("unrecognized option: " <>) nonOpts
  unless (null unrec)   $ Left $ concat unrec
  opts   <- sequenceA eOpts
  msgOpt <- getOption "Messenger" isMsgOption opts
  logOpt <- getOption "Log" isLogOption opts
  return (msgOpt, logOpt)

runChatWith :: SinglMsg m -> SinglLog l -> IO ()
runChatWith msg logger = do
  txt <- T.readFile "config.ini"
  let Right logConfig = Log.getConfig logger txt
  Log.withHandle logger logConfig $ \logH -> do
    let Right config = getConfig txt
    msgConfig <- MSG.getConfig msg logH txt
    dbConfig  <- DB.getConfig msg logH txt
    MSG.withHandle msg msgConfig logH $ \msgH ->
      DB.withHandle dbConfig logH $ \dbH -> 
        forever $ interaction msgH dbH config

main :: IO ()
main = do
  args <- getArgs
  case extractOptions args of
    Left str -> throw $ InputArgsError $ str <> "\n\n" <> usageInfo "Usage info: " options
    Right (MsgOption m, LogOption l)    -> runChatWith m l