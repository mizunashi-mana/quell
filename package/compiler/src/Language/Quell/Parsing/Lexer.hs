module Language.Quell.Parsing.Lexer (
    LexerMonad (..),
    lexerConduit,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Spanned     as Spanned
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Lexing as Lexing
import qualified Language.Quell.Parsing.Lexer.Error as Error
import qualified Language.Quell.Data.Monad.MonadST as MonadST


class (Monad m, MonadST.T s m) => LexerMonad s m where
    reportDecodeError :: Text -> Spanned.BytesSpan -> m ()
    reportLexingError :: Text -> Spanned.Span -> Error.T -> m ()

lexerConduit :: forall s m. LexerMonad s m => Encoding.T
    -> Conduit.ConduitT ByteString (Spanned.T Lexing.LexedUnit) m ()
lexerConduit enc =
    Encoding.decodeConduit enc Conduit..|
    reportDecodeResultConduit Conduit..|
    Lexing.runLexer Lexing.lexer
    where
        reportDecodeResultConduit :: Conduit.ConduitT
            (Spanned.BytesSpan, Encoding.DecodedUnit)
            (Spanned.BytesSpan, Char)
            m ()
        reportDecodeResultConduit = do
            Conduit.await >>= \case
                Nothing ->
                    pure ()
                Just (s, u) -> case u of
                    Encoding.DecodeError msg -> do
                        Conduit.lift do reportDecodeError msg s
                        reportDecodeResultConduit
                    Encoding.DecodedChar c -> do
                        Conduit.yield (s, c)
                        reportDecodeResultConduit
