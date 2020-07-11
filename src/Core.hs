{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core where

import           Data.Ini.Config
import           Data.Update 
import           Data.Text                         (Text)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad                     (replicateM_)
import           Data.ByteString.Char8             (ByteString)
import           Control.Monad
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
             | Done Int
             deriving (Eq, Show)

interaction :: MSG.Handle -> DB.Handle -> Config -> IO ()
interaction msgH dbH cfg@Config{..} = do
  mbN <- DB.getOffset dbH
  lst <- MSG.getUpdate msgH mbN
  forM_ lst $ \ u@Update{..} -> do
    mbN <- DB.getUserRepeatN dbH updUserId
    let n = fromMaybe defRepeatN mbN
        commands = processUpdate n u
    mapM_ (interpretCommand msgH dbH cfg) commands

processUpdate :: Int -> Update -> [Command]
processUpdate n Update{..} = commands <> [Done updId]
  where 
    commands = case updMessage of
      CommandMsg UserCommandHelp -> [SendHelp updUserId]
      CommandMsg UserCommandRepeat -> [SendRepeat updUserId n]
      CommandMsg (UserCommandSetRepeat newN) -> 
          let newMsg = TextMsg $ "Ok, I fix it. It will be " <> S8.show newN
              oldMsg = TextMsg $ "Already " <> S8.show n <> ". There is nothing to change."
          in if newN == n
               then [SendMessage oldMsg updUserId]
               else [SendMessage newMsg updUserId, SetRepeat updUserId newN]
      UnsupportedMsg -> 
          let unMsg  = TextMsg "I`m so sorry. This kind of message is unsupported now."
          in [SendMessage unMsg updUserId]
      msg -> replicate n (SendMessage msg updUserId)

interpretCommand :: MSG.Handle -> DB.Handle -> Config -> Command -> IO ()
interpretCommand msgH dbH Config{..} = \case
  SendHelp userId               -> MSG.sendMessage msgH userId $ TextMsg helpMessage
  SetRepeat userId n            -> DB.setUserRepeatN dbH userId n
  SendMessage msg userId        -> MSG.sendMessage msgH userId msg
  Done updateId                 -> DB.setOffset dbH $ succ updateId
  SendRepeat userId n           -> let msg = TextMsg $ S8.intercalate (S8.show n) . S8.split '@' $ repeatMessage
                                       kb  = [ [("Change to 1", "1"), ("Change to 2", "2")]
                                             , [("Change to 3", "3"), ("Change to 4", "4")]
                                             , [("Change to 5 !!!", "5")]
                                             ]
                                   in MSG.sendKeyMessage msgH kb userId msg