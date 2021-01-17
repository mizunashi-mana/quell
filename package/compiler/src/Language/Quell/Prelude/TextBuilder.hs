module Language.Quell.Prelude.TextBuilder (
    TextBuilder,
    buildStrictText,
    textBuilderFromChar,
    textBuilderFromText,
    textBuilderFromStringLit,
) where

import           Language.Quell.Prelude.Core
import           Language.Quell.Prelude.Literal

import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder


type TextBuilder = TextBuilder.Builder

buildStrictText :: TextBuilder -> Text
buildStrictText b = LazyText.toStrict do TextBuilder.toLazyText b

textBuilderFromChar :: Char -> TextBuilder
textBuilderFromChar c = TextBuilder.singleton c

textBuilderFromText :: Text -> TextBuilder
textBuilderFromText txt = TextBuilder.fromText txt

textBuilderFromStringLit :: StringLit -> TextBuilder
textBuilderFromStringLit str = TextBuilder.fromString str
