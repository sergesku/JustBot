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

options :: [OptDescr (Either ArgsError AppOption)]
options = [ Option ['m'] ["messenger"] (ReqArg readMsgOption "Messenger") "Messenger to run the bot :: tg | vk"
          , Option ['l'] ["log"] (ReqArg readLogOption "Log") "Log output :: console | file"
          ]

trim :: String -> String
trim = f . f 
  where f = reverse . dropWhile isSpace

readMsgOption :: String -> Either ArgsError AppOption
readMsgOption str = case map toUpper trimed of
  "TG" -> Right $ MsgOption STG
  "VK" -> Right $ MsgOption SVK
  _    -> Left  $ UnsapportedOption "Messenger" trimed
  where trimed = trim str

readLogOption :: String -> Either ArgsError AppOption
readLogOption str = case map toUpper trimed of
  "FILE"    -> Right $ LogOption SFile
  "CONSOLE" -> Right $ LogOption SConsole
  _         -> Left  $ UnsapportedOption "Log" trimed
  where trimed = trim str

isMsgOption :: AppOption -> Bool
isMsgOption (MsgOption _ ) = True
isMsgOption _              = False

isLogOption :: AppOption -> Bool
isLogOption (LogOption _) = True
isLogOption _             = False

getOption :: String -> (AppOption -> Bool) -> [AppOption] -> Either ArgsError AppOption
getOption opt f lst = case filter f lst of 
  [x] -> Right x
  []  -> Left $ UnspecifiedOption opt
  _   -> Left $ TooManyOptions opt

extractOptions :: [String] -> Either ArgsError (AppOption, AppOption)
extractOptions args = do
  let (eOpts, nonOpts, unrec) = getOpt RequireOrder options args
  unless (null nonOpts) $ Left $ NonOption $ head nonOpts
  unless (null unrec)   $ Left $ UnrecognizedOption $ head unrec
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
    Left err -> mapM_ putStrLn [show err, "\n" <> usageInfo "Usage info: " options]
    Right (MsgOption m, LogOption l)    -> runChatWith m l