
{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer (
  lexerConduit,
  lexer,
  runLexer,
  LexerMonad (..),
  Lexer (..),
  LexerContext (..),
  LayoutContext (..),
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Lexer.Tlex                as Tlex
import qualified Language.Lexer.Tlex.Data.EnumSet                as EnumSet
import qualified Language.Quell.Parsing.Lexer.Rules as Rules
import qualified Language.Quell.Parsing.Spanned     as Spanned
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Error as Error
import qualified Language.Quell.Type.Token          as Token
import qualified Language.Quell.Data.STBuffer as STBuffer
import qualified Language.Quell.Data.Monad.MonadST as MonadST


$(Rules.buildLexer)

class (Monad m, MonadST.T s m) => LexerMonad s m where
    reportDecodeError :: Text -> Spanned.BytesSpan -> m ()
    reportLexingError :: Text -> Spanned.Span -> Error.T -> m ()

lexerConduit :: forall s m. LexerMonad s m => Encoding.T
    -> Conduit.ConduitT ByteString Token.T m ()
lexerConduit enc =
    Encoding.decodeConduit enc Conduit..|
    reportDecodeResultConduit Conduit..|
    runLexer lexer
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

type CharLexerConduitT = Conduit.ConduitT (Spanned.BytesSpan, Char) Token.T

runLexer :: LexerMonad s m => Lexer s m () -> CharLexerConduitT m ()
runLexer m = do
    ictx <- conduitST buildInitialLexerContext
    Conduit.evalStateC ictx
        do unLexer m

lexer :: forall s m. LexerMonad s m => Lexer s m ()
lexer = go where
    go :: Lexer s m ()
    go = do
        ctx <- Lexer do Conduit.lift get
        result <- tlexScan do lastLexerState ctx
        case result of
            Tlex.TlexEndOfInput ->
                pure ()
            Tlex.TlexError ->
                errorRecoverByTlexScan
            Tlex.TlexAccepted pos act ->
                undefined pos act

    errorRecoverByTlexScan = undefined

newtype Lexer s m a = Lexer
    {
        unLexer :: CharLexerConduitT (StateT (LexerContext s) m) a
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via CharLexerConduitT (StateT (LexerContext s) m)

data LexerContext s = LexerContext
    {
        codeUnitsBuffer        :: STBuffer.T s (Spanned.T (Char, CodeUnit.T)),
        bufferEndOfSource      :: Bool,
        currentPositionContext :: PositionContext,
        layoutContextStack     :: [LayoutContext],
        lastToken              :: Maybe Token.T,
        lastLoc                :: Spanned.Loc,
        lastLexerState         :: Rules.LexerState
    }

buildInitialLexerContext :: ST s (LexerContext s)
buildInitialLexerContext = do
    emptyBuffer <- newEmptyBuffer
    pure do
        LexerContext
            {
                codeUnitsBuffer = emptyBuffer,
                currentPositionContext = PositionContext
                    {
                        bufferPosition = 0
                    },
                bufferEndOfSource = False,
                layoutContextStack = [],
                lastToken = Nothing,
                lastLoc = initialLoc,
                lastLexerState = Rules.Initial
            }
    where
        initialLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            }

newtype PositionContext = PositionContext
    {
        bufferPosition :: Int
    }
    deriving (Eq, Ord, Show)
    deriving Enum via Int

data LayoutContext
    = NoLayout
    | BraceLayout Int
    | DBraceLayout DBraceKind Int
    deriving (Eq, Show)

data DBraceKind
    = DBraceConcrete
    | DBraceVirtualConcrete
    | DBraceVirtual
    deriving (Eq, Ord, Enum, Bounded, Show)

instance LexerMonad s m => Tlex.TlexContext PositionContext CodeUnit.T (Lexer s m) where
    tlexGetInputPart = Lexer do
        ctx <- Conduit.lift get
        let posCtx = currentPositionContext ctx
        mscu <- conduitST do
            codeUnitsBuffer ctx `STBuffer.index` bufferPosition posCtx
        case mscu of
            Just scu -> do
                let (_, u) = Spanned.unSpanned scu
                returnCodeUnit ctx posCtx u
            Nothing
                | bufferEndOfSource ctx ->
                    pure Nothing
                | otherwise ->
                    awaitNewUnit ctx posCtx
        where
            awaitNewUnit ctx posCtx = Conduit.await >>= \case
                Nothing -> do
                    conduitPut do
                        ctx
                            {
                                bufferEndOfSource = True
                            }
                    pure Nothing
                Just (bs, c) -> do
                    let u = CodeUnit.fromChar c
                        buffer = codeUnitsBuffer ctx
                    lastMscu <- conduitST do STBuffer.last buffer
                    let sp = buildSpan lastMscu bs u
                        scu = Spanned.Spanned
                            {
                                Spanned.getSpan = sp,
                                Spanned.unSpanned = (c, u)
                            }
                    conduitST do STBuffer.appendLast scu buffer
                    returnCodeUnit ctx posCtx u

            returnCodeUnit ctx posCtx u = do
                conduitPut do
                    ctx
                        {
                            currentPositionContext = succ posCtx
                        }
                pure do Just u

            buildSpan lastMscu bs u =
                let nlocBytesPos = Spanned.bytesIndex bs + Spanned.bytesLength bs
                in case lastMscu of
                    Just lastScu -> do
                        let lastLoc = Spanned.endLoc do Spanned.getSpan lastScu
                            (_, lastU) = Spanned.unSpanned lastScu
                            loc = lastLoc
                                {
                                    Spanned.locBytesPos = Spanned.bytesIndex bs
                                }
                            nloc = case EnumSet.member u Rules.newlineCs of
                                False -> loc
                                    {
                                        Spanned.locCol = Spanned.locCol loc + 1,
                                        Spanned.locBytesPos = nlocBytesPos
                                    }
                                True
                                    -- "\r\n"
                                    | [lastU, u] == [CodeUnit.LcU000D, CodeUnit.LcU000A] ->
                                        loc
                                            {
                                                Spanned.locBytesPos = nlocBytesPos
                                            }
                                    | otherwise -> loc
                                        {
                                            Spanned.locLine = Spanned.locLine loc + 1,
                                            Spanned.locCol = 0,
                                            Spanned.locBytesPos = nlocBytesPos
                                        }
                        Spanned.Span
                            {
                                Spanned.beginLoc = loc,
                                Spanned.endLoc = nloc
                            }
                    Nothing -> do
                        let loc = Spanned.Loc
                                {
                                    locLine = 0,
                                    Spanned.locCol = 0,
                                    Spanned.locBytesPos = Spanned.bytesIndex bs
                                }
                            nloc = case EnumSet.member u Rules.newlineCs of
                                False -> loc
                                    {
                                        Spanned.locCol = 1,
                                        Spanned.locBytesPos = nlocBytesPos
                                    }
                                True -> loc
                                    {
                                        Spanned.locLine = 1,
                                        Spanned.locBytesPos = nlocBytesPos
                                    }
                        Spanned.Span
                            {
                                Spanned.beginLoc = loc,
                                Spanned.endLoc = nloc
                            }

    tlexGetMark = Lexer do
        ctx <- Conduit.lift get
        pure do currentPositionContext ctx

newEmptyBuffer :: ST s (STBuffer.T s (Spanned.T (Char, CodeUnit.T)))
newEmptyBuffer = STBuffer.new []

conduitST :: MonadST.T s m => ST s a -> Conduit.ConduitT i o m a
conduitST mx = Conduit.lift do MonadST.liftST mx

conduitPut :: Monad m => s -> Conduit.ConduitT i o (StateT s m) ()
conduitPut s = Conduit.lift do put s
