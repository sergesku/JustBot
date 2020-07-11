{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Core where

import           Data.Ini.Config
import           Data.Update 
import           Data.Text                         (Text)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad                     (replicateM_)
import           Data.ByteString.Char8             (ByteString)
import           Control.Monad.Reader              (ReaderT, ask, asks)
import qualified Database                   as DB  
import qualified Messenger                  as MSG 
import qualified Data.Text.Encoding         as T   (encodeUtf8)
import qualified Data.ByteString.Char8.Extended as S8  (split, intercalate, show)
import           Data.Maybe                       (fromMaybe)

data Config = Config
  { helpMessage   :: ByteString
  , repeatMessage :: ByteString
  , defRepeatN    :: Int
  } deriving (Show)

сonfigParser :: IniParser Config
сonfigParser = section "Message" $ do
  helpMessage   <- fieldOf "helpMessage" string
  repeatMessage <- fieldOf "repeatMessage" string
  defRepeatN    <- fieldDefOf "defRepeatNumber" number 1
  return $ Config{..}

getConfig :: Text -> Either String Config
getConfig = (`parseIniFile` сonfigParser)

interaction :: MSG.Handle -> DB.Handle -> ReaderT Config IO ()
interaction msgH dbH = do
  lst <- liftIO $ do mbN <- DB.getOffset dbH
                     MSG.getUpdate msgH mbN
  mapM_ (processUpdate msgH dbH) lst
  
processUpdate :: MSG.Handle -> DB.Handle -> Update -> ReaderT Config IO ()
processUpdate msgH dbH u@Update{..} = do
    case updMessage of
      (CommandMsg Command'Help)          -> processHelp msgHupdUserId
      (CommandMsg Command'Repeat)        -> processRepeat msgH dbHupdUserId
      (CommandMsg (Command'SetRepeat n)) -> processSetRepeat msgH dbHupdUserId n
      _                                  -> processSendAnswer msgH dbH u
    liftIO $ DB.setOffset dbH $ succ updId

processHelp :: MSG.Handle -> UserId -> ReaderT Config IO ()
processHelp msgH userId = asks helpMessage >>= liftIO . MSG.sendMessage msgH userId . TextMsg

processRepeat :: MSG.Handle -> DB.Handle -> UserId -> ReaderT Config IO ()
processRepeat msgH dbH userId = do
    Config{..} <- ask
    n <- liftIO $ fromMaybe defRepeatN <$> DB.getUserRepeatN dbH userId 
    let chunks = S8.split '@' repeatMessage
        msg = TextMsg $ S8.intercalate (S8.show n) chunks
        kb  = [ [("Change to 1", "1"), ("Change to 2", "2")]
              , [("Change to 3", "3"), ("Change to 4", "4")]
              , [("Change to 5 !!!", "5")]
              ]
    liftIO $ MSG.sendKeyMessage msgH kb userId msg

processSetRepeat :: MSG.Handle -> DB.Handle -> UserId -> Int -> ReaderT Config IO ()
processSetRepeat msgH dbH userId newN = do
    defN <- asks defRepeatN
    liftIO $ do
             let newNmessage = TextMsg $ "Ok, I fix it. It will be " <> S8.show newN
                 oldNmessage = TextMsg $ "Already " <> S8.show newN <> ". There is nothing to change."
             mbUserN <- DB.getUserRepeatN dbH userId
             case mbUserN of
               Nothing -> do DB.setUserRepeatN dbH userId newN
                             if newN == defN
                               then MSG.sendMessage msgH userId oldNmessage
                               else MSG.sendMessage msgH userId newNmessage
               Just n  -> if newN == n
                           then MSG.sendMessage msgH userId oldNmessage
                           else do DB.setUserRepeatN dbH userId newN
                                   MSG.sendMessage msgH userId newNmessage
       
processSendAnswer :: MSG.Handle -> DB.Handle -> Update -> ReaderT Config IO ()
processSendAnswer msgH dbH u@Update{..} = do
    defN <- asks defRepeatN
    liftIO $ do n <- fromMaybe defN <$> DB.getUserRepeatN dbHupdUserId
                replicateM_ n $ MSG.sendMessage msgHupdUserId udpMessage

processUpdate :: MSG.Handle -> DB.Handle -> Update -> ReaderT Config IO ()
processUpdate msgH dbH u@Update{..} = do
    case updMessage of
      (CommandMsg Command'Help)          -> processHelp msgHupdUserId
      (CommandMsg Command'Repeat)        -> processRepeat msgH dbHupdUserId
      (CommandMsg (Command'SetRepeat n)) -> processSetRepeat msgH dbHupdUserId n
      _                                  -> processSendAnswer msgH dbH u
    liftIO $ DB.setOffset dbH $ succ updId

data Command = SendHelp UserId
             | SendMessage Message UserId Int 
             | SendRepeat UserId Int
             | SetRepeat UserId Int
             deriving (Eq, Show)