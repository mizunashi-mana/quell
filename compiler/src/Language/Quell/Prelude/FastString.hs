module Language.Quell.Prelude.FastString where

import Language.Quell.Prelude.Core


data FastString = FastString {
  length :: Int,
  bytes  :: ByteString
} deriving Show

instance Eq FastString where
  FastString { bytes = x1 } == FastString { bytes = x2 } = x1 == x2
