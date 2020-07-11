module Data.Update where

import Data.ByteString.Char8 (ByteString)

type UserId = Int

data Update = Update
  { updId      :: Int
  , updUserId  :: UserId
  , udpMessage :: Message
  } deriving (Show)

data Message = TextMsg ByteString
             | FileMsg ByteString
             | PhotoMsg ByteString
             | AudioMsg ByteString
             | StickerMsg ByteString
             | AnimationMsg ByteString
             | ComplexMsg ByteString [Message]
             | CommandMsg Command
             | UnsupportedMsg
             deriving (Show)

data Command = Command'Help
             | Command'Repeat
             | Command'SetRepeat Int
             deriving (Eq, Show)