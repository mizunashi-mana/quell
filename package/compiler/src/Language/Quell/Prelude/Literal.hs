module Language.Quell.Prelude.Literal (
  text,
  byteString,
) where

import qualified Prelude

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text             as Text


{-# INLINE text #-}
text :: Prelude.String -> Text.Text
text = Text.pack

{-# INLINE byteString #-}
byteString :: Prelude.String -> ByteString.ByteString
byteString = ByteString.pack
