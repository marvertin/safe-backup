module Lib
    ( toHexStr,
      yabaSuffix,
      tupleMaybeUpFst,
      tupleMaybeUpSnd, 

    ) where

import qualified Data.ByteString      as Strict
import           Text.Printf          (printf)


yabaSuffix = ".yaba"

class  Hex a  where
  toHexStr :: a -> String

instance Hex Strict.ByteString where
  toHexStr bytes = Strict.unpack bytes >>= printf "%02x"

dropPrefixSlash :: FilePath -> FilePath
dropPrefixSlash ('/' : x) = x
dropPrefixSlash ('\\' : x) = x
dropPrefixSlash x = x

tupleMaybeUpFst :: (Maybe a, b) -> Maybe (a, b)
tupleMaybeUpFst (Nothing, _) = Nothing
tupleMaybeUpFst (Just a, b) = Just (a, b)

tupleMaybeUpSnd :: (a, Maybe b) -> Maybe (a, b)
tupleMaybeUpSnd (_, Nothing) = Nothing
tupleMaybeUpSnd (a, Just b) = Just (a, b)
