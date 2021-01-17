{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing (
  lexer,
  runLexer,
  LexedUnit (..),

  Lexer (..),
  LexerContext (..),
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Lexer.Tlex                as Tlex
import qualified Language.Lexer.Tlex.Data.EnumSet                as EnumSet
import qualified Language.Quell.Parsing.Lexer.Rules as Rules
import qualified Language.Quell.Parsing.Spanned     as Spanned
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Error as Error
import qualified Language.Quell.Type.Token          as Token
import qualified Language.Quell.Data.STBuffer as STBuffer
import qualified Language.Quell.Data.Monad.MonadST as MonadST
import qualified Language.Quell.Data.TextId as TextId


$(Rules.buildLexer)

data LexedUnit
    = LexedToken Token.T
    | LexError Error.T Text
    deriving (Eq, Show)

type LexerConduitT = Conduit.ConduitT
    (Spanned.BytesSpan, Char)
    (Spanned.T LexedUnit)

runLexer :: MonadST.T s m => Lexer s m () -> LexerConduitT m ()
runLexer m = do
    ictx <- conduitST buildInitialLexerContext
    Conduit.evalStateC ictx
        do unLexer m

lexer :: forall s m. MonadST.T s m => Lexer s m ()
lexer = go Rules.Initial where
    go :: Rules.LexerState -> Lexer s m ()
    go lst = do
        result <- tlexScan lst
        case result of
            Tlex.TlexEndOfInput ->
                pure ()
            Tlex.TlexError ->
                yieldError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithToken t ->
                        yieldToken t
                    Rules.WithIdToken t ->
                        yieldIdToken t
                    Rules.WithWhitespace ->
                        consumeBuffer
                    Rules.LexLitIntegerOrRational ->
                        lexAndYieldLitIntegerOrRational
                    Rules.LexLitByteString ->
                        lexAndYieldLitByteString
                    Rules.LexLitByteChar ->
                        lexAndYieldLitByteChar
                    Rules.LexLitString ->
                        lexAndYieldLitString
                    Rules.LexLitChar ->
                        lexAndYieldLitChar
                    Rules.LexInterpStringStart ->
                        lexAndYieldInterpStringStart
                    Rules.LexInterpStringContinue ->
                        lexAndYieldInterpStringContinue
                    Rules.LexCommentLineWithContent ->
                        lexAndYieldCommentLineWithContent
                    Rules.LexCommentMultilineWithContent ->
                        lexAndYieldCommentMultilineWithContent
                    Rules.LexCommentDoc ->
                        lexAndYieldCommentDoc
                    Rules.LexCommentPragma ->
                        lexAndYieldCommentPragma
                go Rules.Initial

newtype Lexer s m a = Lexer
    {
        unLexer :: LexerConduitT (StateT (LexerContext s) m) a
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via LexerConduitT (StateT (LexerContext s) m)

data LexerContext s = LexerContext
    {
        codeUnitsBuffer        :: STBuffer.T s BufferItem,
        bufferEndOfSource      :: Bool,
        currentPositionContext :: PositionContext
    }

type BufferItem = Spanned.T (Char, CodeUnit.T)

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
                bufferEndOfSource = False
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

instance MonadST.T s m => Tlex.TlexContext PositionContext CodeUnit.T (Lexer s m) where
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

setPosition :: Monad m => PositionContext -> Lexer s m ()
setPosition pos = Lexer do
    Conduit.lift do
        modify' \ctx -> ctx
            {
                currentPositionContext = pos
            }

newEmptyBuffer :: ST s (STBuffer.T s (Spanned.T (Char, CodeUnit.T)))
newEmptyBuffer = STBuffer.new []

conduitST :: MonadST.T s m => ST s a -> Conduit.ConduitT i o m a
conduitST mx = Conduit.lift do MonadST.liftST mx

conduitPut :: Monad m => s -> Conduit.ConduitT i o (StateT s m) ()
conduitPut s = Conduit.lift do put s

-- FIXME: try error recovering and report detailed and suggestions
yieldError :: MonadST.T s m => Lexer s m ()
yieldError = Lexer do
    Conduit.yield do undefined do LexError Error.UnexpectedCodeUnits do text "TODO"

yieldToken :: MonadST.T s m => Token.T -> Lexer s m ()
yieldToken = undefined

yieldIdToken :: MonadST.T s m => (TextId.T -> Token.T) -> Lexer s m ()
yieldIdToken = undefined

consumeBuffer :: MonadST.T s m => Lexer s m ()
consumeBuffer = undefined

lexAndYieldLitIntegerOrRational :: MonadST.T s m => Lexer s m ()
lexAndYieldLitIntegerOrRational = undefined

lexAndYieldLitByteString :: MonadST.T s m => Lexer s m ()
lexAndYieldLitByteString = undefined

lexAndYieldLitByteChar :: MonadST.T s m => Lexer s m ()
lexAndYieldLitByteChar = undefined

lexAndYieldLitString :: MonadST.T s m => Lexer s m ()
lexAndYieldLitString = undefined

lexAndYieldLitChar :: MonadST.T s m => Lexer s m ()
lexAndYieldLitChar = undefined

lexAndYieldInterpStringStart :: MonadST.T s m => Lexer s m ()
lexAndYieldInterpStringStart = undefined

lexAndYieldInterpStringContinue :: MonadST.T s m => Lexer s m ()
lexAndYieldInterpStringContinue = undefined

lexAndYieldCommentLineWithContent :: MonadST.T s m => Lexer s m ()
lexAndYieldCommentLineWithContent = undefined

lexAndYieldCommentMultilineWithContent :: MonadST.T s m => Lexer s m ()
lexAndYieldCommentMultilineWithContent = undefined

lexAndYieldCommentDoc :: MonadST.T s m => Lexer s m ()
lexAndYieldCommentDoc = undefined

lexAndYieldCommentPragma :: MonadST.T s m => Lexer s m ()
lexAndYieldCommentPragma = undefined
