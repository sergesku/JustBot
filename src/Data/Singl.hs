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