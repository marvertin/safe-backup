module Lib
    ( toHexStr
    ) where

import qualified Data.ByteString      as Strict
import           Text.Printf          (printf)

class  Hex a  where
  toHexStr :: a -> String

instance Hex Strict.ByteString where
  toHexStr bytes = Strict.unpack bytes >>= printf "%02x"
