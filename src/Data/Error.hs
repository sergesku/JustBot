module Data.Error where

import Control.Exception
import Data.Typeable
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit

data AppError = CodeError String
              | InputArgsError String
              | SystemError SomeException
              | ServiceApiError String
              | ConfigurationError String
              | NetworkError SomeException
              | MessengerResponseTimeout Int
              deriving (Typeable)

instance Exception AppError

instance Show AppError where
  show (CodeError t) = "Error in application code: " <> t
  show (InputArgsError t) = "Input args error:\n" <> t
  show (SystemError e) = "System error: " <> show e
  show (ServiceApiError t) = "Error while communicating with external services: " <> t
  show (ConfigurationError t) = "Error parsing configuration file: " <> t
  show (NetworkError e) = "Network communication error:\n" <> show e
  show (MessengerResponseTimeout t) = "Response timed out: " <> show t <> " seconds."

rethrowHTTPException :: HttpException -> IO a
rethrowHTTPException (HttpExceptionRequest _ (StatusCodeException resp _)) = 
  throw . ServiceApiError . show $ getResponseStatus resp
rethrowHTTPException (HttpExceptionRequest _ ResponseTimeout) = 
  throw MessengerResponseTimeout
rethrowHTTPException e = throw . NetworkError $ toException e