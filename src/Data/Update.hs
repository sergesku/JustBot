module Data.Update where

import Data.ByteString.Char8 (ByteString)

type UserId = Int

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