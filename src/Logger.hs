module Logger where

data Priority = Debug     -- ^ Debug info
              | Info      -- ^ Nottable information that requires no immediate action
              | Warning   -- ^ Something is probably wrong and we should investigate
              | Error     -- ^ Something is wrong and immediate action is required
              deriving (Eq, Ord, Show)