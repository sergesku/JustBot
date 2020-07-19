module Data.Error where

import Control.Exception
import Data.Typeable
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit

data AppError = NetworkError SomeException
              | MessengerResponseTimeout
              | SystemError SomeException
              | InputArgsError String
              | ConfigurationError String
              | ServiceApiError String
              | CodeError String
              deriving (Typeable)

instance Exception AppError

instance Show AppError where
  show MessengerResponseTimeout = "Response timed out"
  show (NetworkError e) = "Network communication error:\n" <> show e
  show (SystemError e) = "System error: " <> show e
  show (InputArgsError t) = "Input args error:\n" <> t
  show (ConfigurationError t) = "Error parsing configuration file: " <> t
  show (ServiceApiError t) = "Error while communicating with external services: " <> t
  show (CodeError t) = "Error in application code: " <> t

rethrowHTTPException :: HttpException -> IO a
rethrowHTTPException (HttpExceptionRequest _ (StatusCodeException resp _)) = 
  throw . ServiceApiError . show $ getResponseStatus resp
rethrowHTTPException (HttpExceptionRequest _ ResponseTimeout) = 
  throw MessengerResponseTimeout
rethrowHTTPException e = throw . NetworkError $ toException e