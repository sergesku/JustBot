{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Messenger.VK where

import           Data.Ini.Config
import           Messenger.Proxy (proxyParser)
import           Data.Update
import           Messenger.Internal
import           Data.Aeson
import           Data.Aeson.Types
import           Network.HTTP.Simple
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
  } deriving (Show)

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
  } deriving (Show)

configParser :: IniParser Config
configParser = section "VK" $ do 
  token     <- fieldOf "token" string
  community <- fieldOf "community" string
  proxy     <- proxyParser
  return $ Config{..}

getConfig :: Text -> Either String Config
getConfig = (`parseIniFile` configParser)

new :: Config -> Handle
new cfg@Config{..} = Handle{..} where
  getUpdate :: Int -> IO [Update]
  getUpdate n = do
    LongPollConfig{..} <- getLongPollConfig cfg
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
    response <- httpLBS req
    let body = getResponseBody response
        eUpdate = parseEither updateLstPars =<< eitherDecode body
    case eUpdate of
        Left e -> error e
        Right lst -> return lst
  
  sendMessage :: UserId -> Content -> IO ()
  sendMessage = sendMessageWith cfg id
  
  sendKeyMessage :: Keyboard -> UserId -> Content -> IO ()
  sendKeyMessage = sendMessageWith cfg . addToRequestQueryString . keyboardQuery

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle cfg f = f $ new cfg

sendMessageWith :: Config -> (Request -> Request) -> UserId -> Content -> IO ()
sendMessageWith Config{..} f userId msg = do
  let baseQuery = [ ("peer_id", Just $ S8.show userId)
                  , ("v", Just "5.89")
                  , ("access_token", Just token)
                  ]      
      query = case msg of
              (TextMsg bs)        -> [("message", Just bs)]
              (StickerMsg bs)     -> [("sticker_id", Just bs)]
              (ComplexMsg bs lst) -> collectQuery bs lst
              m                   -> error $ "Unsupported content type!" <> show m
      req = setRequestMethod "POST"
          $ setRequestProxy proxy
          $ addToRequestQueryString (baseQuery <> query)
          $ "https://api.vk.com/method/messages.send"
  httpLBS $ f req
  return ()
        
collectQuery :: ByteString -> [Content] -> Query
collectQuery txt lst = go txt lst []
    where go t [] result = [ ("message", Just t)
                           , ("attachment", Just $ mconcat $ intersperse "," result)
                           ]
          go t (c:cs) result = case c of
                            PhotoMsg bs     -> go t cs (bs:result)
                            AudioMsg bs     -> go t cs (bs:result)
                            AnimationMsg bs -> go t cs (bs:result)

getLongPollConfig:: Config -> IO LongPollConfig
getLongPollConfig cfg@Config{..} = do
    let query = [ ("group_id", Just community)
                , ("v", Just "5.89")
                , ("access_token", Just token)
                ]
        req = setRequestMethod "POST"
            $ setRequestQueryString query
            $ setRequestProxy proxy
            $ "https://api.vk.com/method/groups.getLongPollServer"
    response <- httpLBS req
    let body    = getResponseBody response
        eServer = eitherDecode body
    print body
    case eServer of
      Left e  -> error e
      Right s -> return s

                     
updateLstPars :: Pars [Update]
updateLstPars = withObject "updateList" $ \o -> do
    nextUpdate <- read <$> (o .: "ts")
    vec <- o .: "updates"
    let size    = V.length vec
        indexed = zip [nextUpdate - size ..] $ V.toList vec
    mapM (uncurry updatePars) indexed

updatePars :: Int -> Pars Update
updatePars updateId = withObject "update" $ \o -> do
    obj     <- o .: "object"
    userId  <- obj .: "user_id"
    content <- asum $ fmap ($ Object obj) [stickerPars, complexPars, commandPars, textPars]
    return $ Update{..}
    
textPars :: Pars Content
textPars = withObject "text" $ \ o -> TextMsg . encodeUtf8 <$> (o .: "body")
 
mediaPars :: (ByteString -> Content) -> String -> Pars Content
mediaPars constructor str = withObject str $ \ o -> do
    obj     <- o .: T.pack str
    ownerId <- (obj .: "owner_id") :: Parser Integer
    mediaId <- (obj .: "id") :: Parser Integer
    key     <- ('_':) <$> (obj .:? "access_key" .!= "")
    return . constructor . S8.pack . mconcat $ [str, show ownerId, "_", show mediaId, key]

photoPars :: Pars Content
photoPars = mediaPars PhotoMsg "photo"

audioPars :: Pars Content
audioPars = mediaPars AudioMsg "audio"

animationPars :: Pars Content
animationPars = mediaPars AnimationMsg "video"

stickerPars :: Pars Content
stickerPars = withObject "sticker" $ \ o -> do
    (x:_)      <- V.toList <$> (o .: "attachments")
    sticker_id <- (x .: "sticker") >>= ( .: "id") :: Parser Int
    return . StickerMsg . S8.show $ sticker_id
    
attachmentPars :: Pars Content
attachmentPars = withObject "attachment" $ \o -> photoPars (Object o)
                                             <|> audioPars (Object o)
                                             <|> animationPars (Object o)

complexPars :: Pars Content
complexPars = withObject "attachmentLst" $ \ o -> do
    txt <- encodeUtf8 <$> (o .: "body")
    vec <- o .: "attachments"
    lst <- mapM attachmentPars $ V.toList vec
    return $ ComplexMsg txt lst

commandPars :: Pars Content
commandPars = withObject "command" $ \ o -> do
    txt <- (o .: "body") :: Parser String
    case words txt of
      ("/help":_)         -> return $ CommandMsg Command'Help
      ("/repeat":_)       -> return $ CommandMsg Command'Repeat
      ("Change":"to":n:_) -> do let x = read n :: Int
                                guard (x > 0 && x <= 5)
                                return $ CommandMsg $ Command'SetRepeat x 
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