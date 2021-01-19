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
    ictx <- Conduit.lift do MonadST.liftST buildInitialLexerContext
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
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithToken t ->
                        yieldToken t
                    Rules.WithIdToken t ->
                        yieldIdToken t
                    Rules.WithWhitespace ->
                        consumeBufferWithNothing
                    Rules.LexLitBitInteger ->
                        lexAndYieldLitBitInteger
                    Rules.LexLitOctitInteger ->
                        lexAndYieldLitOctitInteger
                    Rules.LexLitHexitInteger ->
                        lexAndYieldLitHexitInteger
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
        Monad,
        MonadIO
    ) via LexerConduitT (StateT (LexerContext s) m)

instance MonadST.T s m => MonadST.MonadST s (Lexer s m) where
    type Marker (Lexer s m) = MonadST.Marker m
    liftST mx = Lexer do Conduit.lift do MonadST.liftST mx

data LexerContext s = LexerContext
    {
        codeUnitsBuffer        :: STBuffer.T s BufferItem,
        lastAwaitedSpUnit      :: Maybe BufferItem,
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
                lastAwaitedSpUnit = Nothing,
                currentPositionContext = PositionContext
                    {
                        bufferPosition = 0
                    }
            }

newtype PositionContext = PositionContext
    {
        bufferPosition :: Int
    }
    deriving (Eq, Ord, Show)
    deriving Enum via Int

instance MonadST.T s m => Tlex.TlexContext PositionContext CodeUnit.T (Lexer s m) where
    tlexGetInputPart = do
        ctx <- lexerGet
        let posCtx = currentPositionContext ctx
            buf = codeUnitsBuffer ctx
        mitem <- MonadST.liftST do
            buf `STBuffer.index` bufferPosition posCtx
        case mitem of
            Just item ->
                returnCodeUnit item
            Nothing ->
                awaitNewUnit buf
        where
            awaitNewUnit buf = awaitBufferItem >>= \case
                Nothing ->
                    pure Nothing
                Just item -> do
                    MonadST.liftST do STBuffer.appendLast item buf
                    returnCodeUnit item

            returnCodeUnit item = do
                let (_, u) = Spanned.unSpanned item
                lexerModify \ctx ->
                    ctx
                        {
                            currentPositionContext =
                                succ do currentPositionContext ctx
                        }
                pure do Just u

    tlexGetMark = lexerGet <&> \ctx ->
        currentPositionContext ctx

setPosition :: Monad m => PositionContext -> Lexer s m ()
setPosition pos = lexerModify \ctx -> ctx
    {
        currentPositionContext = pos
    }

awaitBufferItem :: MonadST.T s m => Lexer s m (Maybe BufferItem)
awaitBufferItem = Lexer Conduit.await >>= \case
    Nothing -> do
        pure Nothing
    Just (bs, c) -> do
        ctx <- lexerGet
        let u = CodeUnit.fromChar c
            lastMitem = lastAwaitedSpUnit ctx
            sp = buildSpan lastMitem bs u
            item = Spanned.Spanned
                {
                    getSpan = sp,
                    unSpanned = (c, u)
                }
        lexerPut do
            ctx
                {
                    lastAwaitedSpUnit = Just item
                }
        pure do Just item
    where
        buildSpan lastMitem bs u =
            let nlocBytesPos = Spanned.bytesIndex bs + Spanned.bytesLength bs
            in case lastMitem of
                Just lastItem -> do
                    let lastLoc = Spanned.endLoc do Spanned.getSpan lastItem
                        (_, lastU) = Spanned.unSpanned lastItem
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
                                Spanned.locLine = 0,
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

newEmptyBuffer :: ST s (STBuffer.T s (Spanned.T (Char, CodeUnit.T)))
newEmptyBuffer = STBuffer.new []

lexerGet :: Monad m => Lexer s m (LexerContext s)
lexerGet = Lexer do Conduit.lift get

lexerModify :: Monad m => (LexerContext s -> LexerContext s) -> Lexer s m ()
lexerModify f = Lexer do Conduit.lift do modify' f

lexerPut :: Monad m => LexerContext s -> Lexer s m ()
lexerPut ctx = Lexer do Conduit.lift do put ctx

lexerYield :: Monad m => Spanned.T LexedUnit -> Lexer s m ()
lexerYield u = Lexer do Conduit.yield u

consumeBuffer :: MonadST.T s m => (BufferItem -> a) -> (a -> BufferItem -> a) -> Lexer s m a
consumeBuffer z0f f = do
    ctx <- lexerGet
    let posCtx = currentPositionContext ctx
        buf = codeUnitsBuffer ctx
        pos = bufferPosition posCtx
    case pos of
        0 -> error "unreachable: must be postive position."
        _ -> pure ()
    item0 <- MonadST.liftST do
        STBuffer.unsafeConsumeHead buf
    let z0 = z0f item0
    x <- MonadST.liftST do
        STBuffer.unsafeConsumeHeads z0 f buf do pos - 1
    lexerPut do
        ctx
            {
                currentPositionContext = PositionContext
                    {
                        bufferPosition = 0
                    }
            }
    pure x

consumeBufferItem :: MonadST.T s m => Lexer s m (Maybe BufferItem)
consumeBufferItem = do
    ctx <- lexerGet
    let buf = codeUnitsBuffer ctx
    mitem0 <- MonadST.liftST do
        STBuffer.consumeHead buf
    case mitem0 of
        Just item0 -> pure do Just item0
        Nothing    -> awaitBufferItem

restoreBufferItem :: MonadST.T s m => BufferItem -> Lexer s m ()
restoreBufferItem item = do
    ctx <- lexerGet
    let buf = codeUnitsBuffer ctx
    MonadST.liftST do
        STBuffer.appendHead item buf

lexBufferItem :: MonadST.T s m
    => Spanned.Span -> (CodeUnit.T -> Maybe a) -> Lexer s m (Maybe (Spanned.T a))
lexBufferItem sp f = consumeBufferItem >>= \case
    Nothing -> pure Nothing
    Just item -> do
        let (_, u) = Spanned.unSpanned item
        case f u of
            Nothing -> do
                restoreBufferItem item
                pure Nothing
            Just x  -> pure do
                Just do
                    Spanned.Spanned
                        {
                            getSpan = sp <> Spanned.getSpan item,
                            unSpanned = x
                        }

-- FIXME: try error recovering and report detailed and suggestions
yieldTlexError :: MonadST.T s m => Lexer s m ()
yieldTlexError = lexerYield do
    Spanned.Spanned
        {
            getSpan = undefined,
            unSpanned = LexError Error.UnexpectedCodeUnits
                do text "TODO"
        }

yieldToken :: MonadST.T s m => Token.T -> Lexer s m ()
yieldToken t = do
    sp0 <- consumeBuffer
        do \item ->
            Spanned.getSpan item
        do \sp item ->
            sp <> Spanned.getSpan item
    let u = Spanned.Spanned
            {
                getSpan = sp0,
                unSpanned = LexedToken t
            }
    lexerYield u

yieldIdToken :: MonadST.T s m => (TextId.T -> Token.T) -> Lexer s m ()
yieldIdToken t = do
    sptxtB0 <- consumeBuffer
        do \item -> item <&> \(c, _) -> textBuilderFromChar c
        do \sptxtB item -> do
            let (c, _) = Spanned.unSpanned item
            Spanned.Spanned
                {
                    getSpan =
                        Spanned.getSpan sptxtB <> Spanned.getSpan item,
                    unSpanned =
                        Spanned.unSpanned sptxtB <> textBuilderFromChar c
                }
    let u = sptxtB0 <&> \txtB0 ->
            LexedToken do t do TextId.textId do buildStrictText txtB0
    lexerYield u

consumeBufferWithNothing :: MonadST.T s m => Lexer s m ()
consumeBufferWithNothing = consumeBuffer
    do \_ -> ()
    do \_ _ -> ()

lexAndYieldLitBitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitBitInteger = do
    spIsNegate <- consumeBuffer
        do \item -> item <&> \(_, u) -> isNegateSign u
        do \spIsNegate item -> Spanned.appendSpan
            spIsNegate
            do Spanned.getSpan item
    go0
        do Spanned.unSpanned spIsNegate
        do Spanned.getSpan spIsNegate
    where
        go0 isN sp = do
            mb <- lexBit sp
            case mb of
                Nothing  -> lexerYield do
                    Spanned.Spanned
                        {
                            getSpan = sp,
                            unSpanned = LexError Error.UnconcludedBitIntegerLiteral
                                do text "Expected a bit."
                        }
                Just spb -> go1 isN
                    do Spanned.getSpan spb
                    do Spanned.unSpanned spb

        go1 isN sp i0 = do
            mi1 <- lexBit_ sp i0
            case mi1 of
                Just spi1 -> go1 isN
                    do Spanned.getSpan spi1
                    do Spanned.unSpanned spi1
                Nothing -> do
                    let t = Token.LitInteger case isN of
                            True  -> negate i0
                            False -> i0
                    lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp,
                                unSpanned = LexedToken t
                            }

        lexBit sp = lexBufferItem sp \case
            CodeUnit.LcUNum0 -> Just 0
            CodeUnit.LcUNum1 -> Just 1
            _                -> Nothing

        lexBit_ sp i0 = lexBufferItem sp \case
            CodeUnit.LcUSymUnscore  -> Just do i0
            CodeUnit.LcUNum0        -> Just do i0 * 2
            CodeUnit.LcUNum1        -> Just do i0 * 2 + 1
            _                       -> Nothing

lexAndYieldLitOctitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitOctitInteger = undefined

lexAndYieldLitHexitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitHexitInteger = undefined

lexAndYieldLitIntegerOrRational :: MonadST.T s m => Lexer s m ()
lexAndYieldLitIntegerOrRational = undefined

isNegateSign :: CodeUnit.T -> Bool
isNegateSign = \case
    CodeUnit.LcU002D -> True
    _                -> False

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
