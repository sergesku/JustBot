{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messenger.Internal where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Update
import           Data.Text                       (Text)

type Button   = (Text, Text)        -- (Button Label, Button Value)
type Keyboard = [[Button]]
type Pars a = Value -> Parser a

data Handle = Handle
  { getUpdate      :: Int -> IO [Update]
  , sendMessage    :: UserId -> Content -> IO ()
  , sendKeyMessage :: Keyboard -> UserId -> Content -> IO () 
  }

