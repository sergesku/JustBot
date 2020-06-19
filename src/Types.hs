{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Types where

import Data.ByteString.Char8 (ByteString, pack, unpack)

type UserId = Int

data Messenger = TG
               | VK
               deriving (Show)

data Singl (m :: Messenger) where
  STG :: Singl 'TG
  SVK :: Singl 'VK

data Update = Update
  { updateId :: Int
  , userId   :: UserId
  , content  :: Content
  } deriving (Show)

data Content = TextMsg ByteString
             | FileMsg ByteString
             | PhotoMsg ByteString
             | AudioMsg ByteString
             | StickerMsg ByteString
             | AnimationMsg ByteString
             | ComplexMsg ByteString [Content]
             | CommandMsg Command
             | UnsupportedMsg
             deriving (Show)

data Command = Command'Help
             | Command'Repeat
             | Command'SetRepeat Int
             deriving (Eq, Show)