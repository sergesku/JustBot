{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Messenger 
  ( Handle(..)
  , getConfig
  , withHandle
  )
  where

import           Data.Singl
import           Data.Text                (Text)
import           Data.Update
import           Messenger.Internal       (Handle(..))
import qualified Messenger.VK       as VK
import qualified Messenger.TG       as TG

type family Config' (m :: Msg) :: *
type instance Config' 'TG = TG.Config
type instance Config' 'VK = VK.Config

getConfig :: SinglMsg m -> Text -> Either String (Config' m)
getConfig STG = TG.getConfig
getConfig SVK = VK.getConfig

withHandle :: SinglMsg m -> Config' m -> (Handle -> IO ()) -> IO ()
withHandle STG = TG.withHandle
withHandle SVK = VK.withHandle

new :: SinglMsg m -> Config' m -> Handle
new STG = TG.new
new SVK = VK.new