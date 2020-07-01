module Logger.Internal where

import Prelude hiding (log)
import Control.Monad.Reader

type AppMonad = ReaderT Handle IO

data Priority = Debug     -- ^ Debug info
              | Info      -- ^ Nottable information that requires no immediate action
              | Warning   -- ^ Something is probably wrong and we should investigate
              | Error     -- ^ Something is wrong and immediate action is required
              deriving (Eq, Ord, Show, Read)

newtype Handle = Handle { log :: Priority -> String -> IO () }

logDebug :: String -> AppMonad ()
logDebug = logPriority Debug

logInfo :: String -> AppMonad ()
logInfo = logPriority Info

logWarning :: String -> AppMonad ()
logWarning = logPriority Warning

logError :: String -> AppMonad ()
logError = logPriority Error

logPriority :: Priority -> String -> AppMonad ()
logPriority pri str = do
  h <- ask
  let message = mkLogMessage pri str
  liftIO $ log h pri message

mkLogMessage :: Priority -> String -> String
mkLogMessage  p str = unwords [show p, ":", str]