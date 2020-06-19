module Logger.Internal where

import Prelude hiding (log)

data Priority = Debug     -- ^ Debug info
              | Info      -- ^ Nottable information that requires no immediate action
              | Warning   -- ^ Something is probably wrong and we should investigate
              | Error     -- ^ Something is wrong and immediate action is required
              deriving (Eq, Ord, Show)

newtype Handle = Handle { log :: Priority -> String -> IO () }

logDebug :: Handle -> String -> IO ()
logDebug = (`log` Debug)

logInfo :: Handle -> String -> IO ()
logInfo = (`log` Info)

logWarning :: Handle -> String -> IO ()
logWarning = (`log` Warning)

logError :: Handle -> String -> IO ()
logError = (`log` Error)

mkLogMessage :: Priority -> String -> String
mkLogMessage  p str = unwords [show p, ":", str]