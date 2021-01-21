module Language.Quell.Parsing.Lexer (
    LexerMonad (..),
    lexerConduit,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Data.Monad.MonadST     as MonadST
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Error    as Error
import qualified Language.Quell.Parsing.Lexer.Lexing   as Lexing
import qualified Language.Quell.Parsing.Spanned        as Spanned
import qualified Language.Quell.Type.Token             as Token


class (Monad m, MonadST.T s m) => LexerMonad s m where
    reportDecodeError :: Spanned.BytesSpan -> Text -> m ()
    reportLexError :: Spanned.Span -> Error.T -> Text -> m ()

instance LexerMonad RealWorld IO where
    reportDecodeError bsp txt = do
        let biS = Spanned.bytesIndex bsp
        putStr do show biS
        putStr ":"
        putTextLn do txt

    reportLexError sp err txt = do
        let locS = Spanned.beginLoc sp
        putStr do show do Spanned.locLine locS
        putStr ":"
        putStr do show do Spanned.locCol locS
        putStr ":"
        putStr do show err
        putStr " - "
        putTextLn do txt

lexerConduit :: forall s m. LexerMonad s m => Encoding.T
    -> Conduit.ConduitT ByteString (Spanned.T Token.T) m ()
lexerConduit enc =
    Encoding.decodeConduit enc
    Conduit..| reportDecodeResultConduit
    Conduit..| Lexing.runLexer Lexing.lexer
    Conduit..| reportLexResultConduit
    where
        reportDecodeResultConduit :: Conduit.ConduitT
            (Spanned.BytesSpan, Encoding.DecodedUnit)
            (Spanned.BytesSpan, Char)
            m ()
        reportDecodeResultConduit = do
            Conduit.await >>= \case
                Nothing ->
                    pure ()
                Just (s, u) -> debugTraceShow (s, u) case u of
                    Encoding.DecodeError msg -> do
                        Conduit.lift do reportDecodeError s msg
                        reportDecodeResultConduit
                    Encoding.DecodedChar c -> do
                        Conduit.yield (s, c)
                        reportDecodeResultConduit

        reportLexResultConduit :: Conduit.ConduitT
            (Spanned.T Lexing.LexedUnit)
            (Spanned.T Token.T)
            m ()
        reportLexResultConduit = do
            Conduit.await >>= \case
                Nothing ->
                    pure ()
                Just spu -> case Spanned.unSpanned spu of
                    Lexing.LexError err msg -> do
                        Conduit.lift do
                            reportLexError
                                do Spanned.getSpan spu
                                err msg
                        reportLexResultConduit
                    Lexing.LexedToken t -> do
                        Conduit.yield do spu <&> \_ -> t
                        reportLexResultConduit
