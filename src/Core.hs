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

data Command = SendHelp UserId
             | SendMessage Message UserId
             | SendRepeat UserId Int
             | SetRepeat UserId Int
             | SendUnsapportedAnswer UserId
             deriving (Eq, Show)

processUpdate' :: Update       -- ^ Update to process
               -> Int          -- ^ Default reps
               -> Maybe Int    -- ^ Reps for current user
               -> [Command]    -- ^ Commands for executions
processUpdate' u@Update{..} defN mbN =
  case updMessage of
    UnsapportedMsg                      -> [SendUnsapportedAnswer updUserId]
    CommandMsg Command'Help             -> [SendHelp updUserId]
    CommandMsg Command'Repeat           -> [SendRepeat updUserId oldN]
    CommandMsg (Command'SetRepeat newN) -> if oldN == newN 
                                            then [SendMessage oldMsg updUserId]
                                            else [SendMessage newMsg updUserId, SetRepeat updUserId newN]
    msg                                 -> replicate oldN $ SendMessage msg updUserId
  where oldN   = fromMaybe n mbN
        newMsg = TextMsg $ "Ok, I fix it. It will be " <> S8.show newN
        oldMsg = TextMsg $  "Already " <> S8.show oldN <> ". There is nothing to change."

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

