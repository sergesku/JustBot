{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Messenger 
  ( Handle(..)
  , getConfig
  , withHandle
  )
  where

import           Data.Text                (Text)
import           Types
import           Messenger.Internal       (Handle(..))
import qualified Messenger.VK       as VK
import qualified Messenger.TG       as TG

type family Config' (m :: Messenger) :: *
type instance Config' 'TG = TG.Config
type instance Config' 'VK = VK.Config

getConfig :: Singl m -> Text -> Either String (Config' m)
getConfig STG = TG.getConfig
getConfig SVK = VK.getConfig

withHandle :: Singl m -> Config' m -> (Handle -> IO ()) -> IO ()
withHandle STG = TG.withHandle
withHandle SVK = VK.withHandle

new :: Singl m -> Config' m -> Handle
new STG = TG.new
new SVK = VK.new