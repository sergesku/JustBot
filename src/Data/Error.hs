module Data.Error where

import Control.Exception
import Data.Typeable

data AppError = NetworkError SomeException
              | SystemError SomeException
              | InputArgsError String
              | ConfigurationError String
              | ServiceApiError String
              deriving (Typeable)

instance Exception AppError

instance Show AppError where
  show (NetworkError e) = "Network communication error: " <> show e
  show (SystemError e) = "System error: " <> show e
  show (InputArgsError t) = "Unsapported args: " <> t
  show (ConfigurationError t) = "Error parsing configuration file: " <> t
  show (ServiceApiError t) = "Error while communicating with external services: " <> t