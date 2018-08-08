module Types (
  FileSize,
  Hash,
  JabaContent
) where

import qualified Data.ByteString       as Strict

type FileSize = Integer
type JabaContent = String

type Hash = Strict.ByteString
