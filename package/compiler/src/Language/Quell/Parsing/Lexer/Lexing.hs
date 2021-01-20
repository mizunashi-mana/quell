{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing where

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

lexBufferItemWithChar :: MonadST.T s m
    => Spanned.Span -> (Char -> CodeUnit.T -> Maybe a) -> Lexer s m (Maybe (Spanned.T a))
lexBufferItemWithChar sp f = consumeBufferItem >>= \case
    Nothing -> pure Nothing
    Just item -> do
        let (c, u) = Spanned.unSpanned item
        case f c u of
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

lexBufferItem :: MonadST.T s m
    => Spanned.Span -> (CodeUnit.T -> Maybe a) -> Lexer s m (Maybe (Spanned.T a))
lexBufferItem sp f = lexBufferItemWithChar sp \_ -> f

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
        go0 isN sp = lexBit sp >>= \case
            Just spi -> go1 isN
                do Spanned.getSpan spi
                do toInteger do Spanned.unSpanned spi
            Nothing  -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp,
                        unSpanned = LexError Error.UnconcludedBitIntegerLiteral
                            do text "Expected a bit."
                    }

        go1 isN sp i0 = lexBit_ sp i0 >>= \case
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

        lexBit sp = lexBufferItem sp \u -> case lexBitUnit u of
            Nothing -> Nothing
            Just -1 -> Nothing
            Just n  -> Just n

        lexBit_ sp i0 = lexBufferItem sp \u -> case lexBitUnit u of
            Nothing -> Nothing
            Just -1 -> Just i0
            Just n  -> Just do i0 * 0b10 + toInteger n

        lexBitUnit = \case
            CodeUnit.LcUSymUnscore  -> Just @Int -1
            CodeUnit.LcUNum0        -> Just 0
            CodeUnit.LcUNum1        -> Just 1
            _                       -> Nothing

lexAndYieldLitOctitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitOctitInteger = do
    spIsNegate <- consumeBuffer
        do \item -> item <&> \(_, u) -> isNegateSign u
        do \spIsNegate item -> Spanned.appendSpan
            spIsNegate
            do Spanned.getSpan item
    go0
        do Spanned.unSpanned spIsNegate
        do Spanned.getSpan spIsNegate
    where
        go0 isN sp = lexOctit sp >>= \case
            Just spi -> go1 isN
                do Spanned.getSpan spi
                do toInteger do Spanned.unSpanned spi
            Nothing  -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp,
                        unSpanned = LexError Error.UnconcludedOctitIntegerLiteral
                            do text "Expected a octit."
                    }

        go1 isN sp i0 = lexOctit_ sp i0 >>= \case
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

        lexOctit sp = lexBufferItem sp \u -> case lexOctitUnit u of
            Nothing -> Nothing
            Just -1 -> Nothing
            Just n  -> Just n

        lexOctit_ sp i0 = lexBufferItem sp \u -> case lexOctitUnit u of
            Nothing -> Nothing
            Just -1 -> Just i0
            Just n  -> Just do i0 * 0o10 + toInteger n

        lexOctitUnit = \case
            CodeUnit.LcUSymUnscore  -> Just @Int -1
            CodeUnit.LcUNum0        -> Just 0
            CodeUnit.LcUNum1        -> Just 1
            CodeUnit.LcUNum2        -> Just 2
            CodeUnit.LcUNum3        -> Just 3
            CodeUnit.LcUNum4        -> Just 4
            CodeUnit.LcUNum5        -> Just 5
            CodeUnit.LcUNum6        -> Just 6
            CodeUnit.LcUNum7        -> Just 7
            _                       -> Nothing

lexAndYieldLitHexitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitHexitInteger = do
    spIsNegate <- consumeBuffer
        do \item -> item <&> \(_, u) -> isNegateSign u
        do \spIsNegate item -> Spanned.appendSpan
            spIsNegate
            do Spanned.getSpan item
    go0
        do Spanned.unSpanned spIsNegate
        do Spanned.getSpan spIsNegate
    where
        go0 isN sp = lexHexit sp >>= \case
            Just spi -> go1 isN
                do Spanned.getSpan spi
                do toInteger do Spanned.unSpanned spi
            Nothing  -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp,
                        unSpanned = LexError Error.UnconcludedHexitIntegerLiteral
                            do text "Expected a hexit."
                    }

        go1 isN sp i0 = lexHexit_ sp i0 >>= \case
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

        lexHexit sp = lexBufferItemWithChar sp \c u ->
            case lexHexitUnit c u of
                Nothing -> Nothing
                Just -1 -> Nothing
                Just n  -> Just n

        lexHexit_ sp i0 = lexBufferItemWithChar sp \c u ->
            case lexHexitUnit c u of
                Nothing -> Nothing
                Just -1 -> Just i0
                Just n  -> Just do i0 * 0x10 + toInteger n

        lexHexitUnit c u = case u of
            CodeUnit.LcUSymUnscore  -> Just -1
            CodeUnit.LcUSmallAlphaA -> Just 0xA
            CodeUnit.LcUSmallAlphaB -> Just 0xB
            CodeUnit.LcUSmallAlphaC -> Just 0xC
            CodeUnit.LcUSmallAlphaD -> Just 0xD
            CodeUnit.LcUSmallAlphaE -> Just 0xE
            CodeUnit.LcUSmallAlphaF -> Just 0xF
            CodeUnit.LcULargeAlphaA -> Just 0xA
            CodeUnit.LcULargeAlphaB -> Just 0xB
            CodeUnit.LcULargeAlphaC -> Just 0xC
            CodeUnit.LcULargeAlphaD -> Just 0xD
            CodeUnit.LcULargeAlphaE -> Just 0xE
            CodeUnit.LcULargeAlphaF -> Just 0xF
            _                       -> lexDigitChar c

lexAndYieldLitIntegerOrRational :: MonadST.T s m => Lexer s m ()
lexAndYieldLitIntegerOrRational = undefined

isNegateSign :: CodeUnit.T -> Bool
isNegateSign = \case
    CodeUnit.LcU002D -> True
    _                -> False

-- | decode digit
--
-- * https://www.compart.com/en/unicode/category/Nd
lexDigitChar :: Char -> Maybe Int
lexDigitChar c = let i = fromEnum c in if
    | 0x00030 <= i && i <= 0x00039 -> Just do i - 0x00030
    | 0x00660 <= i && i <= 0x00669 -> Just do i - 0x00660
    | 0x006F0 <= i && i <= 0x006F9 -> Just do i - 0x006F0
    | 0x007C0 <= i && i <= 0x007C9 -> Just do i - 0x007C0
    | 0x00966 <= i && i <= 0x0096F -> Just do i - 0x00966
    | 0x009E6 <= i && i <= 0x009EF -> Just do i - 0x009E6
    | 0x00A66 <= i && i <= 0x00A6F -> Just do i - 0x00A66
    | 0x00AE6 <= i && i <= 0x00AEF -> Just do i - 0x00AE6
    | 0x00B66 <= i && i <= 0x00B6F -> Just do i - 0x00B66
    | 0x00BE6 <= i && i <= 0x00BEF -> Just do i - 0x00BE6
    | 0x00C66 <= i && i <= 0x00C6F -> Just do i - 0x00C66
    | 0x00CE6 <= i && i <= 0x00CEF -> Just do i - 0x00CE6
    | 0x00D66 <= i && i <= 0x00D6F -> Just do i - 0x00D66
    | 0x00DE6 <= i && i <= 0x00DEF -> Just do i - 0x00DE6
    | 0x00E50 <= i && i <= 0x00E59 -> Just do i - 0x00E50
    | 0x00ED0 <= i && i <= 0x00ED9 -> Just do i - 0x00ED0
    | 0x00F20 <= i && i <= 0x00F29 -> Just do i - 0x00F20
    | 0x01040 <= i && i <= 0x01049 -> Just do i - 0x01040
    | 0x01090 <= i && i <= 0x01099 -> Just do i - 0x01090
    | 0x017E0 <= i && i <= 0x017E9 -> Just do i - 0x017E0
    | 0x01810 <= i && i <= 0x01819 -> Just do i - 0x01810
    | 0x01946 <= i && i <= 0x0194F -> Just do i - 0x01946
    | 0x019D0 <= i && i <= 0x019D9 -> Just do i - 0x019D0
    | 0x01A80 <= i && i <= 0x01A89 -> Just do i - 0x01A80
    | 0x01A90 <= i && i <= 0x01A99 -> Just do i - 0x01A90
    | 0x01B50 <= i && i <= 0x01B59 -> Just do i - 0x01B50
    | 0x01BB0 <= i && i <= 0x01BB9 -> Just do i - 0x01BB0
    | 0x01C40 <= i && i <= 0x01C49 -> Just do i - 0x01C40
    | 0x01C50 <= i && i <= 0x01C59 -> Just do i - 0x01C50
    | 0x0A620 <= i && i <= 0x0A629 -> Just do i - 0x0A620
    | 0x0A8D0 <= i && i <= 0x0A8D9 -> Just do i - 0x0A8D0
    | 0x0A900 <= i && i <= 0x0A909 -> Just do i - 0x0A900
    | 0x0A9D0 <= i && i <= 0x0A9D9 -> Just do i - 0x0A9D0
    | 0x0A9F0 <= i && i <= 0x0A9F9 -> Just do i - 0x0A9F0
    | 0x0AA50 <= i && i <= 0x0AA59 -> Just do i - 0x0AA50
    | 0x0ABF0 <= i && i <= 0x0ABF9 -> Just do i - 0x0ABF0
    | 0x0FF10 <= i && i <= 0x0FF19 -> Just do i - 0x0FF10
    | 0x104A0 <= i && i <= 0x104A9 -> Just do i - 0x104A0
    | 0x10D30 <= i && i <= 0x10D39 -> Just do i - 0x10D30
    | 0x11066 <= i && i <= 0x1106F -> Just do i - 0x11066
    | 0x110F0 <= i && i <= 0x110F9 -> Just do i - 0x110F0
    | 0x11136 <= i && i <= 0x1113F -> Just do i - 0x11136
    | 0x111D0 <= i && i <= 0x111D9 -> Just do i - 0x111D0
    | 0x112F0 <= i && i <= 0x112F9 -> Just do i - 0x112F0
    | 0x11450 <= i && i <= 0x11459 -> Just do i - 0x11450
    | 0x114D0 <= i && i <= 0x114D9 -> Just do i - 0x114D0
    | 0x11650 <= i && i <= 0x11659 -> Just do i - 0x11650
    | 0x116C0 <= i && i <= 0x116C9 -> Just do i - 0x116C0
    | 0x11730 <= i && i <= 0x11739 -> Just do i - 0x11730
    | 0x118E0 <= i && i <= 0x118E9 -> Just do i - 0x118E0
    | 0x11C50 <= i && i <= 0x11C59 -> Just do i - 0x11C50
    | 0x11D50 <= i && i <= 0x11D59 -> Just do i - 0x11D50
    | 0x11DA0 <= i && i <= 0x11DA9 -> Just do i - 0x11DA0
    | 0x16A60 <= i && i <= 0x16A69 -> Just do i - 0x16A60
    | 0x16B50 <= i && i <= 0x16B59 -> Just do i - 0x16B50
    | 0x1D7CE <= i && i <= 0x1D7D7 -> Just do i - 0x1D7CE
    | 0x1D7D8 <= i && i <= 0x1D7E1 -> Just do i - 0x1D7D8
    | 0x1D7E2 <= i && i <= 0x1D7EB -> Just do i - 0x1D7E2
    | 0x1D7EC <= i && i <= 0x1D7F5 -> Just do i - 0x1D7EC
    | 0x1D7F6 <= i && i <= 0x1D7FF -> Just do i - 0x1D7F6
    | 0x1E140 <= i && i <= 0x1E149 -> Just do i - 0x1E140
    | 0x1E2F0 <= i && i <= 0x1E2F9 -> Just do i - 0x1E2F0
    | 0x1E950 <= i && i <= 0x1E959 -> Just do i - 0x1E950
    | otherwise                    -> Nothing

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
