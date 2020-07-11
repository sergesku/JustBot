module Data.Update where

import Data.ByteString.Char8 (ByteString)

type UserId = Int

data Update = Update
  { updId      :: Int
  , updUserId  :: UserId
  , updMessage :: Message
  } deriving (Show)

data Message = TextMsg ByteString
             | FileMsg ByteString
             | PhotoMsg ByteString
             | AudioMsg ByteString
             | StickerMsg ByteString
             | AnimationMsg ByteString
             | ComplexMsg ByteString [Message]
             | CommandMsg UserCommand
             | UnsupportedMsg
             deriving (Eq, Show)

data UserCommand = UserCommandHelp
                 | UserCommandRepeat
                 | UserCommandSetRepeat Int
                 deriving (Eq, Show)