module Data.Error where

import Control.Exception
import Data.Typeable

data AppError = ErrorDB DatabaseError
              | ErrorLog LogError
              | ErrorMsg MessengerError
              | ErrorCfg ConfigError
              deriving (Show, Typeable)

instance Exception AppError

data DatabaseError = DatabaseReadError String
                   | DatabaseWriteError String
                   deriving Show

data LogError = WriteLogError String
                deriving Show

data MessengerError = MessengerError deriving Show

data ConfigError = MessengerConfigError String
                | LoggingConfigError String
                | DatabaseConfigError String
                | ApplicationConfigError String
                deriving Show