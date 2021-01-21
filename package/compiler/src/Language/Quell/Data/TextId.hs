module Language.Quell.Data.TextId (
  T,
  TextId (..),
  textId,
  stringLit,
  showByText,
) where

import           Language.Quell.Prelude


type T = TextId

newtype TextId = UnsafeTextId
    {
        unsafeUnTextId :: Text -- FIXME: Use memorized hash integer
    }
    deriving (Eq, Show)

textId :: Text -> TextId
textId txt = UnsafeTextId do txt

stringLit :: StringLit -> TextId
stringLit str = textId do text str

-- FIXME: Reference memorized hash table
showByText :: TextId -> Text
showByText = \case
  UnsafeTextId t -> t

-- FIXME: Consider comparing memorized hash integer mechanizm
instance Ord TextId where
  compare = coerce do compare @Text

instance Pretty TextId where
  pretty t = pretty do showByText t
