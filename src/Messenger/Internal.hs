{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messenger.Internal where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Update
import           Data.Text                       (Text)
import           Network.HTTP.Simple

type Button   = (Text, Text)        -- (Button Label, Button Value)
type Keyboard = [[Button]]
type Pars a = Value -> Parser a

data Handle = Handle
  { getUpdate      :: Int -> IO [Update]
  , sendMessage    :: UserId -> Message -> IO ()
  , sendKeyMessage :: Keyboard -> UserId -> Message -> IO () 
  }

showResponse :: (Show a) => Response a -> String
showResponse resp = unlines
  [ "Response {"
  , "  status    = " ++ show status
  , "  body      = " ++ show body
  , "}"
  ]
  where status = getResponseStatus resp
        body   = getResponseBody resp