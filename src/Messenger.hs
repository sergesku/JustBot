{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Messenger 
  ( Handle(..)
  , getConfig
  , withHandle
  )
  where

import qualified Logger
import           Data.Singl
import           Data.Text                (Text)
import           Data.Update
import           Messenger.Internal       (Handle(..))
import qualified Messenger.VK       as VK
import qualified Messenger.TG       as TG

type family Config' (m :: Msg) :: *
type instance Config' 'TG = TG.Config
type instance Config' 'VK = VK.Config

getConfig :: SinglMsg m -> Logger.Handle -> Text -> IO (Config' m)
getConfig STG = TG.getConfig
getConfig SVK = VK.getConfig

withHandle :: SinglMsg m -> Config' m -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle STG = TG.withHandle
withHandle SVK = VK.withHandle