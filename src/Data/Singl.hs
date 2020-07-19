{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Singl where

data Msg = TG
         | VK
         deriving (Show)

data Log = FileLog 
         | ConsoleLog
         deriving (Eq, Show)

data SinglMsg (m :: Msg) where
  STG :: SinglMsg 'TG
  SVK :: SinglMsg 'VK

data SinglLog (l :: Log) where
  SFile    :: SinglLog 'FileLog
  SConsole :: SinglLog 'ConsoleLog

instance Show (SinglMsg (m :: Msg)) where
  show STG = "STG"
  show SVK = "SVK"

instance Show (SinglLog (l :: Log)) where
  show SFile    = "SFile"
  show SConsole = "SConsole"