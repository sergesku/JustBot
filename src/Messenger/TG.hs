{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Messenger.TG where

import           Data.Error
import           Control.Exception
import qualified Logger
import           Messenger.Proxy (proxyParser)
import           Network.HTTP.Simple
import           Data.Update
import           Messenger.Internal
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative        
import           Control.Monad 
import           Control.Monad.Reader
import           Data.Foldable               (asum)
import           Data.ByteString.Char8       (ByteString)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Text                   (Text)
import qualified Data.Vector           as V
import qualified Data.Text             as T
import qualified Data.ByteString.Char8.Extended as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Ini.Config

data Config = Config
  { token :: ByteString
  , proxy :: Maybe Proxy
  } deriving (Show)               

configParser :: IniParser Config
configParser = section "TG" $ do 
  token <- fieldOf "token" string
  proxy <- proxyParser
  return $ Config{..}

getConfig :: Logger.Handle -> Text -> IO Config
getConfig logH txt = do
  Logger.logDebug logH "Messenger | Reading TG config from file config.ini"
  let eConfig = parseIniFile txt configParser
  case eConfig of
    Right cfg -> do Logger.logDebug logH $ "Messenger | TG Config: " <> show cfg
                    return cfg
    Left err  -> do Logger.logError logH $ "Messenger | Error parsing configuration file: " <> err
                    throw $ ConfigurationError err

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle cfg@Config{..} logH f = f Handle{..} where
  sendMessage :: UserId -> Message -> IO ()
  sendMessage = sendMessageWith id
  
  sendKeyMessage :: Keyboard -> UserId -> Message -> IO ()
  sendKeyMessage  = sendMessageWith . addToRequestQueryString . keyboardQuery
  
  getUpdate :: Int -> IO [Update]
  getUpdate offset = handle rethrowHTTPException $ do
    let query = [("offset", Just $ S8.show offset), ("timeout", Just "25")]
        req   = baseReqWith "GET" "/getUpdates" query
    Logger.logDebug logH $ "Messenger | Sending <Get Updates> request:\n" <> show req
    response <- httpLBS req
    let body   = getResponseBody response
        update = parseEither updateLstPars =<< eitherDecode body
    Logger.logDebug logH $ "Messenger | Response <Get Updates> received:\n" <> show response
    case update of
      Left err  -> do Logger.logWarning logH $ "Messenger | Error parsing updates from JSON: " <> err
                      throw $ ServiceApiError err
      Right lst -> do Logger.logDebug logH $ "Messenger | Received updates:\n " <> show lst
                      return lst

  sendMessageWith :: (Request -> Request) -> UserId -> Message -> IO ()
  sendMessageWith f userId cont = handle rethrowHTTPException $ 
    case eReq of
    Right req -> do Logger.logDebug logH $ "Messenger | Sending request: \n" <> show req
                    void $ httpLBS $ f req
    Left err  -> do Logger.logWarning logH $ "Messenger | " <> err
                    throw $ ServiceApiError err
    where postReqWith path (flag,txt) = baseReqWith "POST" path [("chat_id", Just $ S8.show userId), (flag, Just txt)]
          eReq = case cont of
                  (TextMsg t)      -> Right $ postReqWith "/sendMessage"   ("text", t)
                  (FileMsg t)      -> Right $ postReqWith "/sendDocument"  ("document", t)
                  (AudioMsg t)     -> Right $ postReqWith "/sendVoice"     ("voice", t)
                  (StickerMsg t)   -> Right $ postReqWith "/sendSticker"   ("sticker", t)
                  (AnimationMsg t) -> Right $ postReqWith "/sendAnimation" ("animation", t)
                  (PhotoMsg t)     -> Right $ postReqWith "/sendPhoto"     ("photo", t)
                  unsupported      -> Left $ "Unsupported TG content type: " <> show unsupported

  baseReqWith :: ByteString -> ByteString -> Query -> Request
  baseReqWith method path query = setRequestMethod method
                                $ setRequestPath ("bot" <> token <> path)
                                $ addToRequestQueryString query
                                $ setRequestProxy proxy
                                $ parseRequestThrow_ "https://api.telegram.org"

keyboardQuery :: Keyboard -> Query
keyboardQuery kb = [("reply_markup", Just . L8.toStrict . encode $ keyboard kb)]
  where keyboard :: Keyboard -> Value
        keyboard kb = object [ "inline_keyboard" .= (fmap.fmap) button kb]
        button :: Button -> Value
        button (label, value) = object [ "text" .= label
                                       , "callback_data" .= ("/setRepeat " <> value)
                                       ]


-- | This section voted to parsing Update value from response.

updateLstPars :: Pars [Update]
updateLstPars = withObject "[Update] with Bot.Impl.TG.updateLstPars" $ \o -> do
  vec <- o .: "result"
  mapM updatePars $ V.toList vec
    
updatePars :: Pars Update
updatePars = withObject "Update with Bot.Impl.TG.updatePars" $ \o -> do
    updId      <- (o .: "update_id") :: Parser Int
    message    <- o .: "message" <|> o .: "edited_message" <|> o .: "callback_query"
    updUserId  <- (message .: "from") >>= ( .: "id")
    updMessage <- asum [ commandPars (Object message)
                     , callbackPars (Object message)
                     , TextMsg . encodeUtf8 <$> (message .: "text")
                     , AudioMsg . encodeUtf8 <$> ((message .: "voice") >>= ( .: "file_id"))
                     , AnimationMsg . encodeUtf8 <$> ((message .: "animation") >>= ( .: "file_id"))
                     , FileMsg . encodeUtf8 <$> ((message .: "document") >>= ( .: "file_id"))
                     , StickerMsg . encodeUtf8 <$> ((message .: "sticker") >>= ( .: "file_id"))
                     , photoPars (Object message)
                     , return UnsupportedMsg ]   
    return Update{..}

photoPars :: Pars Message
photoPars = withObject "Photo with Bot.Impl.TG.photoPars" $ \o -> do
    (x:_) <- V.toList <$> (o .: "photo")
    PhotoMsg . encodeUtf8 <$> (x .: "file_id")

callbackPars :: Pars Message
callbackPars = withObject "Command with Bot.Impl.TG.callbackPars" $ \o -> do
    str <- (o .: "data") :: Parser String
    case words str of
      ("/setRepeat":n:_) -> return $ CommandMsg $ Command'SetRepeat (read n)
      _                  -> fail "Wrong Callback"

commandPars :: Pars Message
commandPars = withObject "Command with Bot.Impl.TG.commandPars" $ \o -> do
    (x:_ )     <- (o .: "entities")
    entityType <- withObject "Entity type with Bot.Impl.TG.commandPars" ( .: "type") x :: Parser String
    guard (entityType == "bot_command")
    (txt:_) <- words <$> (o .: "text")
    case txt of
        "/help"       -> return $ CommandMsg Command'Help
        "/repeat"     -> return $ CommandMsg Command'Repeat
        _             -> fail $ "Unsupported Command"