module Language.Quell.Prelude.Text (
    putText,
    putTextLn,
    TextBuilder,
    buildText,
    textBuilderFromChar,
    textBuilderFromText,
    textBuilderFromStringLit,
) where

import           Language.Quell.Prelude.Core
import           Language.Quell.Prelude.Literal

import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as LazyText
import qualified Data.Text.Lazy.Builder         as TextBuilder


putText :: Text -> IO ()
putText txt = Text.putStr txt

putTextLn :: Text -> IO ()
putTextLn txt = Text.putStrLn txt

type TextBuilder = TextBuilder.Builder

buildText :: TextBuilder -> Text
buildText b = LazyText.toStrict do TextBuilder.toLazyText b

textBuilderFromChar :: Char -> TextBuilder
textBuilderFromChar c = TextBuilder.singleton c

textBuilderFromText :: Text -> TextBuilder
textBuilderFromText txt = TextBuilder.fromText txt

textBuilderFromStringLit :: StringLit -> TextBuilder
textBuilderFromStringLit str = TextBuilder.fromString str
