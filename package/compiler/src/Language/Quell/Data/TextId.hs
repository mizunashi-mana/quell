module Language.Quell.Data.TextId (
  T,
  TextId (..),
  textId,
  showByText,
) where

import           Language.Quell.Prelude

import qualified Data.Text.Encoding     as TextEnc


type T = TextId

newtype TextId = UnsafeTextId
  { unsafeUnTextId :: Text -- FIXME: Use memorized hash integer
  } deriving (Eq, Show)

textId :: ByteString -> TextId
textId bs = UnsafeTextId do TextEnc.decodeUtf8 bs

-- FIXME: Reference memorized hash table
showByText :: TextId -> Text
showByText = \case
  UnsafeTextId t -> t

-- FIXME: Consider comparing memorized hash integer mechanizm
instance Ord TextId where
  compare = coerce do compare @Text

instance Pretty TextId where
  pretty t = pretty do showByText t
