{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Core where

import           Logger                            (AppMonad)
import           Data.Ini.Config
import           Data.Update
import           Data.Text                         (Text)
import           Control.Monad.Trans.Class         (lift)
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

interaction :: MSG.Handle -> DB.Handle -> ReaderT Config AppMonad ()
interaction msgH dbH = do
  lst <- lift $ DB.getOffset dbH >>= MSG.getUpdate msgH
  mapM_ (processUpdate msgH dbH) lst
  
processUpdate :: MSG.Handle -> DB.Handle -> Update -> ReaderT Config AppMonad ()
processUpdate msgH dbH u@Update{..} = do
    case content of
      (CommandMsg Command'Help)          -> processHelp msgH userId
      (CommandMsg Command'Repeat)        -> processRepeat msgH dbH userId
      (CommandMsg (Command'SetRepeat n)) -> processSetRepeat msgH dbH userId n
      _                                  -> processSendAnswer msgH dbH u
    lift $ DB.setOffset dbH $ succ updateId

processHelp :: MSG.Handle -> UserId -> ReaderT Config AppMonad ()
processHelp msgH userId = asks helpMessage >>= lift . MSG.sendMessage msgH userId . TextMsg

processRepeat :: MSG.Handle -> DB.Handle -> UserId -> ReaderT Config AppMonad ()
processRepeat msgH dbH userId = do
    Config{..} <- ask
    n <- lift $ fromMaybe defRepeatN <$> DB.getUserRepeatN dbH userId 
    let chunks = S8.split '@' repeatMessage
        msg = TextMsg $ S8.intercalate (S8.show n) chunks
        kb  = [ [("Change to 1", "1"), ("Change to 2", "2")]
              , [("Change to 3", "3"), ("Change to 4", "4")]
              , [("Change to 5 !!!", "5")]
              ]
    lift $ MSG.sendKeyMessage msgH kb userId msg

processSetRepeat :: MSG.Handle -> DB.Handle -> UserId -> Int -> ReaderT Config AppMonad ()
processSetRepeat msgH dbH userId newN = do
    defN <- asks defRepeatN
    lift $ do
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
       
processSendAnswer :: MSG.Handle -> DB.Handle -> Update -> ReaderT Config AppMonad ()
processSendAnswer msgH dbH u@Update{..} = do
    defN <- asks defRepeatN
    lift $ do n <- fromMaybe defN <$> DB.getUserRepeatN dbH userId
              replicateM_ n $ MSG.sendMessage msgH userId content