{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

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

options :: [OptDescr AppOption]
options = [ Option ['m'] ["messenger"] (ReqArg getMsgOption "Messenger") "Messenger to run the bot :: tg | vk"
          , Option ['l'] ["log"] (ReqArg getLogOption "Log") "Log output :: console | file"
          ]

processStr :: String -> String
processStr = map toUpper . trim
  where trim = f . f
        f    = reverse . dropWhile isSpace

getMsgOption :: String -> AppOption
getMsgOption str = case processStr str of
  "TG" -> MsgOption STG
  "VK" -> MsgOption SVK
  _    -> throwInputArgsError $ "Unsapported Messenger type: " <> str <> "."

getLogOption :: String -> AppOption
getLogOption str = case processStr str of
  "FILE"    -> LogOption SFile
  "CONSOLE" -> LogOption SConsole
  _         -> throwInputArgsError $ "Unsapported Log type: " <> str <> "."

isMsgOption :: AppOption -> Bool
isMsgOption (MsgOption _ ) = True
isMsgOption _              = False

isLogOption :: AppOption -> Bool
isLogOption (LogOption _) = True
isLogOption _             = False

withMsgOption :: AppOption -> (forall m. SinglMsg m -> r) -> r
withMsgOption (MsgOption x) f = f x

withLogOption :: AppOption -> (forall l. SinglLog l -> r) -> r
withLogOption (LogOption l) f = f l

getOption :: String -> (AppOption -> Bool) -> [AppOption] -> AppOption
getOption opt f lst = case filter f lst of 
  [x] -> x
  []  -> throwInputArgsError $ "You didn`t specify which " <> opt <> " to use."
  _   -> throwInputArgsError $ "Too many " <> opt <> " options specified."

throwInputArgsError :: String -> a
throwInputArgsError = throw . InputArgsError . withUsage
  where withUsage str = str <> "\n\n" <> usageInfo "Usage info: " options

main = do
  args <- getArgs
  let (opts, nonOpts, unrec) = getOpt RequireOrder options args
      msgOpt = getOption "Messenger" isMsgOption opts
      logOpt = getOption "Log" isLogOption opts
  unless (null nonOpts) $ throwInputArgsError $ unlines $ map ("unrecognized option: " <>) nonOpts
  unless (null unrec)   $ throwInputArgsError $ concat unrec
  withLogOption logOpt $ withMsgOption msgOpt runChat

runChat :: SinglMsg m -> SinglLog l -> IO ()
runChat msg logger = do
    txt <- T.readFile "config.ini"
    let Right logConfig = Log.getConfig logger txt
    Log.withHandle logger logConfig $ \logH -> do
      let Right config = getConfig txt
      msgConfig <- MSG.getConfig msg logH txt
      dbConfig  <- DB.getConfig msg logH txt
      MSG.withHandle msg msgConfig logH $ \msgH ->
        DB.withHandle dbConfig logH $ \dbH -> 
          forever $ interaction msgH dbH config