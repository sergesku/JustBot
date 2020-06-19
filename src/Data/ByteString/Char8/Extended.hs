module Data.ByteString.Char8.Extended
  ( module Data.ByteString.Char8
  , show
  , read
  )
where

import           Data.ByteString.Char8
import qualified Prelude                        ( show, read )
import           Prelude                 hiding ( show, read )

show :: Show a => a -> ByteString
show = pack . Prelude.show

read :: Read a => ByteString -> a
read = Prelude.read . unpack
