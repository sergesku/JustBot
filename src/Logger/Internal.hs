module Logger.Internal where

import Prelude hiding (log)

data Priority = Debug     -- ^ Debug info
              | Info      -- ^ Nottable information that requires no immediate action
              | Warning   -- ^ Something is probably wrong and we should investigate
              | Error     -- ^ Something is wrong and immediate action is required
              deriving (Eq, Ord, Show, Read)

newtype Handle = Handle { log :: Priority -> String -> IO () }

logDebug :: Handle -> String -> IO ()
logDebug = (`logPriority` Debug)

logInfo :: Handle -> String -> IO ()
logInfo = (`logPriority` Info)

logWarning :: Handle -> String -> IO ()
logWarning = (`logPriority` Warning)

logError :: Handle -> String -> IO ()
logError = (`logPriority` Error)

logPriority :: Handle -> Priority -> String -> IO ()
logPriority h pri = log h pri . mkLogMessage pri

mkLogMessage :: Priority -> String -> String
mkLogMessage  p str = unwords [show p, ":", str]