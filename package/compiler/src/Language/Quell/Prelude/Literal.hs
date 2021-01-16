module Language.Quell.Prelude.Literal (
    StringLit,
    text,
    byteString,
) where

import qualified Prelude

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text             as Text


type StringLit = Prelude.String

{-# INLINE text #-}
text :: StringLit -> Text.Text
text = Text.pack

{-# INLINE byteString #-}
byteString :: StringLit -> ByteString.ByteString
byteString = ByteString.pack
