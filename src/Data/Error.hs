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
              | ResponseTimeoutError
              deriving (Typeable)

instance Exception AppError

instance Show AppError where
  show (CodeError t) = "Error in application code: " <> t
  show (InputArgsError t) = "Input args error:\n" <> t
  show (SystemError e) = "System error: " <> show e
  show (ServiceApiError t) = "Error while communicating with external services:\n" <> t
  show (ConfigurationError t) = "Error parsing configuration file: " <> t
  show (NetworkError e) = "Network communication error:\n" <> show e
  show ResponseTimeoutError = "Messenger response timed out."

rethrowHTTPException :: HttpException -> IO a
rethrowHTTPException err = throw appError
  where 
    appError = case err of 
      (InvalidUrlException url reason) -> ServiceApiError $ unwords [reason,"<", url, ">"]
      (HttpExceptionRequest _ (StatusCodeException resp _)) -> ServiceApiError . show $ getResponseStatus resp
      (HttpExceptionRequest _ ResponseTimeout) -> ResponseTimeoutError
      e -> NetworkError $ toException e