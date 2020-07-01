{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Messenger 
  ( Handle(..)
  , getConfig
  , withHandle
  )
  where

import           Logger                   (AppMonad)
import           Data.Singl
import           Data.Text                (Text)
import           Data.Update
import           Messenger.Internal       (Handle(..))
import qualified Messenger.VK       as VK
import qualified Messenger.TG       as TG

type family Config' (m :: Msg) :: *
type instance Config' 'TG = TG.Config
type instance Config' 'VK = VK.Config

getConfig :: SinglMsg m -> Text -> AppMonad (Config' m)
getConfig STG = TG.getConfig
getConfig SVK = VK.getConfig

withHandle :: SinglMsg m -> Config' m -> (Handle -> AppMonad ()) -> AppMonad ()
withHandle STG = TG.withHandle
withHandle SVK = VK.withHandle