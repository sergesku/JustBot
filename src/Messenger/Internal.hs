{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messenger.Internal where

import           Logger                          (AppMonad)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Update
import           Data.Text                       (Text)

type Button   = (Text, Text)        -- (Button Label, Button Value)
type Keyboard = [[Button]]
type Pars a = Value -> Parser a

data Handle = Handle
  { getUpdate      :: Int -> AppMonad [Update]
  , sendMessage    :: UserId -> Content -> AppMonad ()
  , sendKeyMessage :: Keyboard -> UserId -> Content -> AppMonad () 
  }

