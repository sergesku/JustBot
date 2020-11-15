module Data.Error where

import Control.Exception
import Data.Typeable
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit
import Data.Char (toUpper)

data AppError = CodeError String
              | SystemError SomeException
              | ServiceApiError String
              | ConfigurationError String
              | NetworkError SomeException
              | ResponseTimeoutError
              deriving (Typeable)

data ArgsError = UnsapportedOption String String
               | UnspecifiedOption String
               | TooManyOptions String
               | UnrecognizedOption String
               | NonOption String

instance Exception AppError

instance Show AppError where
  show (CodeError t) = unlines ["Error in application code:", t]
  show (SystemError e) = unlines ["System error:\n", show e]
  show (ServiceApiError t) = unlines ["Error while communicating with external services:\n", t]
  show (ConfigurationError t) = unlines ["Error parsing configuration file:", t]
  show (NetworkError e) = unlines ["Network communication error:\n", show e]
  show ResponseTimeoutError = "Messenger response timed out."

instance Show ArgsError where
  show (UnsapportedOption opt str) = "Input args error: Unsapported " <> opt <> " type: '" <> str <> "'."
  show (UnspecifiedOption opt) = "Input args error: You didn`t specify which " <> opt <> " to use."
  show (TooManyOptions opt) = "Input args error: Too many " <> opt <> " options specified."
  show (UnrecognizedOption str) = "Input args error: " <> headToUpper str
  show (NonOption str) = "Input args error: '" <> str <> "' is not a proper option"

rethrowHTTPException :: HttpException -> IO a
rethrowHTTPException err = throwIO appError
  where 
    appError = case err of 
      (InvalidUrlException url reason) -> ServiceApiError $ unwords [reason,"<", url, ">"]
      (HttpExceptionRequest _ (StatusCodeException resp _)) -> ServiceApiError . show $ getResponseStatus resp
      (HttpExceptionRequest _ ResponseTimeout) -> ResponseTimeoutError
      e -> NetworkError $ toException e

rethrowIOException :: IOException -> IO a
rethrowIOException = throwIO . SystemError . toException

headToUpper :: String -> String
headToUpper [] = []
headToUpper (x:xs) = toUpper x : xs