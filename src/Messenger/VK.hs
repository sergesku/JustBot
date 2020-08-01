{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Messenger.VK where

import           Data.Error
import qualified Logger
import           Data.Ini.Config
import           Messenger.Proxy (proxyParser)
import           Data.Update
import           Messenger.Internal
import           Data.Aeson
import           Data.Aeson.Types
import           Network.HTTP.Simple
import           Control.Exception
import           Control.Monad.Reader
import           Data.ByteString.Char8       (ByteString)
import           Data.Text.Encoding          (encodeUtf8)
import           Control.Applicative
import           Data.Foldable               (asum)
import           Data.Text                   (Text)
import           Data.List                   (intersperse)
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.ByteString.Char8.Extended as S8
import qualified Data.ByteString.Lazy.Char8 as L8

data LongPollConfig = LongPollConfig
  { lpKey    :: ByteString
  , lpServer :: ByteString
  , lpOffset :: Int
  }

instance Show LongPollConfig where
  show LongPollConfig{..} = unlines
    [ "LongPollConfig {"
    , "  lpKey        = " ++ show lpKey
    , "  lpServer     = " ++ show lpServer
    , "  lpOffset     = " ++ show lpOffset
    , "}"
    ]

instance FromJSON LongPollConfig where
  parseJSON = withObject "LongPollConfig" $ \ o -> do
    resp     <- o .: "response"
    lpKey    <- encodeUtf8 <$> (resp .: "key")
    lpServer <- encodeUtf8 <$> (resp .: "server")
    lpOffset <- read <$> (resp .: "ts")
    return $ LongPollConfig{..}

data Config = Config
  { token      :: ByteString
  , community  :: ByteString
  , proxy      :: Maybe Proxy
  }

instance Show Config where
  show Config{..} = unlines
    [ "Config {"
    , "  token        = " ++ show token
    , "  community    = " ++ show community
    , "  proxy        = " ++ show proxy
    , "}"
    ]

configParser :: IniParser Config
configParser = section "VK" $ do 
  token     <- fieldOf "token" string
  community <- fieldOf "community" string
  proxy     <- proxyParser
  return $ Config{..}

getConfig :: Logger.Handle -> Text -> IO Config
getConfig logH txt = do
  let eConfig = parseIniFile txt configParser
  case eConfig of
    Right cfg -> do Logger.logDebug logH $ "Messenger | Read VK config from file config.ini:\n" <> show cfg
                    return cfg
    Left err  -> do Logger.logError logH $ "Messenger | Couldn`t read VK config from file config.ini." <> err
                    throw $ ConfigurationError err

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle  cfg@Config{..} logH f = f Handle{..} where

  sendMessage :: UserId -> Message -> IO ()
  sendMessage = sendMessageWith id
 
  sendKeyMessage :: Keyboard -> UserId -> Message -> IO ()
  sendKeyMessage = sendMessageWith . addToRequestQueryString . keyboardQuery
  
  getUpdate :: Int -> IO [Update]
  getUpdate n = handle rethrowHTTPException $ do
    LongPollConfig{..} <- getLongPollConfig cfg logH
    let lastN n = lpOffset - n
        offset = max (lastN 20) n
        query  = [ ("act", Just "a_check")
                 , ("key", Just lpKey)
                 , ("ts", Just $ S8.show offset)
                 , ("wait", Just "25")
                 ]
    req <- setRequestQueryString query
           <$> setRequestProxy proxy
           <$> (parseRequest $ S8.unpack lpServer)
    Logger.logDebug logH $ "Messenger | Sending <Get Updates> request:\n" <> show req
    response <- httpLBS req
    Logger.logDebug logH $ "Messenger | Response <Get Updates> received:\n" <> showResponse response
    let body = getResponseBody response
        eUpdate = parseEither updateLstPars =<< eitherDecode body
    case eUpdate of
      Left err  -> do Logger.logWarning logH $ "Messenger | Error parsing updates from JSON: " <> err
                      throw $ ServiceApiError err
      Right lst -> do Logger.logDebug logH $ "Messenger | Updates received:\n" <> show lst
                      return lst
  

  sendMessageWith :: (Request -> Request) -> UserId -> Message -> IO ()
  sendMessageWith f userId msg = handle rethrowHTTPException $ do
    let baseQuery = [ ("peer_id", Just $ S8.show userId)
                    , ("v", Just "5.89")
                    , ("access_token", Just token)
                    ]      
        eQuery = case msg of
                (TextMsg bs)        -> Right [("message", Just bs)]
                (StickerMsg bs)     -> Right [("sticker_id", Just bs)]
                (ComplexMsg bs lst) -> Right $ collectQuery bs lst
                unsupported         -> Left $ "Unsupported VK content type: " <> show unsupported
    case eQuery of
      Left err  -> Logger.logWarning logH $ "Messenger | " <> err
      Right q   -> do let req = setRequestMethod "POST"
                              $ setRequestProxy proxy
                              $ addToRequestQueryString (baseQuery <> q)
                              $ "https://api.vk.com/method/messages.send"
                      Logger.logDebug logH $ "Messenger | Sending <Post Message> request:\n" <> show req
                      void $ httpLBS $ f req

        
collectQuery :: ByteString -> [Message] -> Query
collectQuery txt lst = go txt lst []
    where go t [] result = [ ("message", Just t)
                           , ("attachment", Just $ mconcat $ intersperse "," result)
                           ]
          go t (c:cs) result = case c of
                            PhotoMsg bs     -> go t cs (bs:result)
                            AudioMsg bs     -> go t cs (bs:result)
                            AnimationMsg bs -> go t cs (bs:result)

getLongPollConfig:: Config -> Logger.Handle -> IO LongPollConfig
getLongPollConfig cfg@Config{..} logH = do
    let query = [ ("group_id", Just community)
                , ("v", Just "5.89")
                , ("access_token", Just token)
                ]
        req = setRequestMethod "POST"
            $ setRequestQueryString query
            $ setRequestProxy proxy
            $ "https://api.vk.com/method/groups.getLongPollServer"
    Logger.logDebug logH $ "Messenger | Sending <Get LongPoll Config> request:\n" <> show req
    response <- httpLBS req
    Logger.logDebug logH $ "Messenger | Responce <Get LongPoll Config> received:\n" <> showResponse response
    let body = getResponseBody response
        eSrv = eitherDecode body
    case eSrv of
      Left e  -> do Logger.logError logH "Messenger | Couldn`t parse LongPoll Config from responce"
                    throw $ ServiceApiError "Couldn`t parse LongPoll Config from responce"
      Right s -> do Logger.logDebug logH $ "Messenger | Get LongPoll Config from responce:\n" <> show s
                    return s
 
updateLstPars :: Pars [Update]
updateLstPars = withObject "updateList" $ \o -> do
    nextUpdate <- read <$> (o .: "ts")
    vec <- o .: "updates"
    let size    = V.length vec
        indexed = zip [nextUpdate - size ..] $ V.toList vec
    mapM (uncurry updatePars) indexed

updatePars :: Int -> Pars Update
updatePars updId = withObject "update" $ \o -> do
  obj     <- o .: "object"
  updUserId  <- obj .: "user_id"
  updMessage <- asum $ fmap ($ Object obj) [stickerPars, complexPars, commandPars, textPars]
  return $ Update{..}
    
textPars :: Pars Message
textPars = withObject "text" $ \ o -> TextMsg . encodeUtf8 <$> (o .: "body")
 
mediaPars :: (ByteString -> Message) -> String -> Pars Message
mediaPars constructor str = withObject str $ \ o -> do
    obj     <- o .: T.pack str
    ownerId <- (obj .: "owner_id") :: Parser Integer
    mediaId <- (obj .: "id") :: Parser Integer
    key     <- ('_':) <$> (obj .:? "access_key" .!= "")
    return . constructor . S8.pack . mconcat $ [str, show ownerId, "_", show mediaId, key]

photoPars :: Pars Message
photoPars = mediaPars PhotoMsg "photo"

audioPars :: Pars Message
audioPars = mediaPars AudioMsg "audio"

animationPars :: Pars Message
animationPars = mediaPars AnimationMsg "video"

stickerPars :: Pars Message
stickerPars = withObject "sticker" $ \ o -> do
    (x:_)      <- V.toList <$> (o .: "attachments")
    sticker_id <- (x .: "sticker") >>= ( .: "id") :: Parser Int
    return . StickerMsg . S8.show $ sticker_id
    
attachmentPars :: Pars Message
attachmentPars = withObject "attachment" $ \o -> photoPars (Object o)
                                             <|> audioPars (Object o)
                                             <|> animationPars (Object o)

complexPars :: Pars Message
complexPars = withObject "attachmentLst" $ \ o -> do
    txt <- encodeUtf8 <$> (o .: "body")
    vec <- o .: "attachments"
    lst <- mapM attachmentPars $ V.toList vec
    return $ ComplexMsg txt lst

commandPars :: Pars Message
commandPars = withObject "command" $ \ o -> do
    txt <- (o .: "body") :: Parser String
    case words txt of
      ("/help":_)         -> return $ CommandMsg UserCommandHelp
      ("/repeat":_)       -> return $ CommandMsg UserCommandRepeat
      ("Change":"to":n:_) -> do let x = read n :: Int
                                guard (x > 0 && x <= 5)
                                return $ CommandMsg $ UserCommandSetRepeat x 
      _                   -> fail "Unsupported command"


keyboardQuery :: Keyboard -> Query
keyboardQuery kb = [("keyboard", Just . L8.toStrict . encode $ keyboard kb)]
  where keyboard :: Keyboard -> Value
        keyboard kb = object [ "inline"   .= False
                             , "buttons"  .= (fmap.fmap) button kb
                             , "one_time" .= True
                             ]
        button :: Button -> Value
        button (label,_) = object [ "color"  .= ("secondary" :: Text)
                                  , "action" .= object [ "type" .= ("text" :: Text)
                                                       , "label"   .= label 
                                                       ]
                                  ]