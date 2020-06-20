-- | Specific section, voted to the parsing and validation of Proxy configuration from the configuration file.
-- | It`s created for use in any instance of Bot_Handle. Each Bot_Handle implementation can use it`s own Proxy.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger.Proxy
  ( proxyParser
  ) where

import           Data.Update
import           Data.Ini.Config
import           Text.Read                   (readMaybe)
import           Control.Monad.Except
import           Network.HTTP.Simple         (Proxy(..))
import           Data.Text                   (Text)
import qualified Data.Text             as T  (unpack, words, splitOn)
import           Data.ByteString.Char8 as S8 (pack)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.Text.Encoding    as T      (encodeUtf8)

newtype Validate e a = Validate {runValidate :: Either e a}
                       deriving (Show, Functor, Monad)

instance Monoid e => Applicative (Validate e) where
  pure = Validate . Right
  Validate (Left e1) <*> Validate (Left e2) = Validate . Left $ e1 <> e2
  Validate (Left e1) <*> _ = Validate . Left $ e1
  Validate (Right f) <*> Validate a = Validate $ f <$> a

instance Monoid e => MonadError e (Validate e) where
  throwError = Validate . Left
  Validate (Left e) `catchError` h = h e
  v `catchError` _ = v

proxyParser :: SectionParser (Maybe Proxy)
proxyParser = fieldMbOf "proxy" $ \txt -> 
  let eProxy = runValidate $ validateProxy txt
   in case eProxy of
        Left lst -> Left $ concatMap T.unpack lst
        Right p  -> Right p
            
validateProxy :: Text -> Validate [Text] Proxy
validateProxy txt = 
  case T.words txt of
   [ip,port]   -> Proxy <$> validateIp ip <*> validatePort port
   ip:port:rest -> throwError ["\nIncorrect Proxy format: " <> txt] <*> validateIp ip <*> validatePort port
   _            -> throwError ["\nIncorrect Proxy format: " <> txt]

checkOctet :: String -> Maybe ()
checkOctet oct = do i <- readMaybe oct
                    guard (i >= 0 && i < 255)

validateIp :: Text -> Validate [Text] ByteString
validateIp ip = do
  let octs = T.splitOn "." ip
      result = traverse (checkOctet . T.unpack) octs
  case result of
    Nothing -> throwError ["\nIncorrect IP format: " <> ip]
    Just _  -> return $ T.encodeUtf8 ip

validatePort :: Text -> Validate [Text] Int
validatePort port = do
  let result = readMaybe (T.unpack port) >>= \p -> guard (p > 0) >> return p
  case result of
    Nothing -> throwError ["\nIncorrect Port format: " <> port]
    Just p  -> return p
