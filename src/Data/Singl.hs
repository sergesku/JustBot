{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Singl where

data Msg = TG
               | VK
               deriving (Show)

data SinglMsg (m :: Msg) where
  STG :: SinglMsg 'TG
  SVK :: SinglMsg 'VK