{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Data.ByteString.Builder                     as BSBuilder
import qualified Data.ByteString.Lazy                        as LazyByteString
import qualified Language.Lexer.Tlex                         as Tlex
import qualified Language.Lexer.Tlex.Data.EnumSet            as EnumSet
import qualified Language.Quell.Data.Monad.MonadST           as MonadST
import qualified Language.Quell.Data.STBuffer                as STBuffer
import qualified Language.Quell.Data.TextId                  as TextId
import qualified Language.Quell.Parsing.Lexer.CodeUnit       as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Error          as Error
import qualified Language.Quell.Parsing.Lexer.Lexing.CharEsc as CharEscLex
import qualified Language.Quell.Parsing.Lexer.Rules          as Rules
import qualified Language.Quell.Parsing.Spanned              as Spanned
import qualified Language.Quell.Type.Token                   as Token


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
            Tlex.TlexEndOfInput -> debugTrace "scan: end of input" do
                pure ()
            Tlex.TlexError -> debugTrace "scan: error" do
                yieldTlexError
            Tlex.TlexAccepted pos act -> debugTrace "scan: accepted" do
                setPosition pos
                case act of
                    Rules.WithToken t -> debugTrace ("scan: with token: " <> show t) do
                        yieldToken t
                    Rules.WithIdToken t -> debugTrace "scan: with id token" do
                        yieldIdToken t
                    Rules.WithWhitespace -> debugTrace "scan: with whitespace" do
                        consumeBufferWithNothing
                    Rules.LexLitRationalWithDot -> debugTrace "scan: lex rational with dot" do
                        lexAndYieldLitRationalWithDot
                    Rules.LexLitRationalWithoutDot -> debugTrace "scan: lex rational without dot" do
                        lexAndYieldLitRationalWithoutDot
                    Rules.LexLitBitInteger -> debugTrace "scan: lex bit integer" do
                        lexAndYieldLitBitInteger
                    Rules.LexLitOctitInteger -> debugTrace "scan: lex octit integer" do
                        lexAndYieldLitOctitInteger
                    Rules.LexLitHexitInteger -> debugTrace "scan: lex hexit integer" do
                        lexAndYieldLitHexitInteger
                    Rules.LexLitDecimalInteger -> debugTrace "scan: lex decimal integer" do
                        lexAndYieldLitDecimalInteger
                    Rules.LexLitByteString -> debugTrace "scan: lex byte string" do
                        lexAndYieldLitByteString
                    Rules.LexLitByteChar -> debugTrace "scan: lex byte char" do
                        lexAndYieldLitByteChar
                    Rules.LexLitString -> debugTrace "scan: lex string" do
                        lexAndYieldLitString
                    Rules.LexLitChar -> debugTrace "scan: lex char" do
                        lexAndYieldLitChar
                    Rules.LexInterpStringStart -> debugTrace "scan: lex interp string" do
                        lexAndYieldInterpStringStart
                    Rules.LexInterpStringContinue -> debugTrace "scan: lex interp string continue" do
                        lexAndYieldInterpStringContinue
                    Rules.LexCommentLineWithContent -> debugTrace "scan: lex line comment" do
                        lexAndYieldCommentLineWithContent
                    Rules.LexCommentMultilineWithContent -> debugTrace "scan: lex multiline comment" do
                        lexAndYieldCommentMultilineWithContent
                    Rules.LexCommentDoc -> debugTrace "scan: lex doc comment" do
                        lexAndYieldCommentDoc
                    Rules.LexCommentPragma -> debugTrace "scan: lex pragma comment" do
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

consumeBufferItem :: forall s m. MonadST.T s m => Lexer s m (Maybe BufferItem)
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

yieldIdToken :: MonadST.T s m => Rules.IdToken -> Lexer s m ()
yieldIdToken (Rules.IdToken t) = do
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

consumeBufferWithSpan :: forall s m. MonadST.T s m => Lexer s m Spanned.Span
consumeBufferWithSpan = consumeBuffer
    do \item -> Spanned.getSpan item
    do \sp item -> sp <> Spanned.getSpan item

consumeBufferWithNothing :: MonadST.T s m => Lexer s m ()
consumeBufferWithNothing = consumeBuffer
    do \_ -> ()
    do \_ _ -> ()

data LexItemState a = LexItemState a
    (a -> Char -> CodeUnit.T -> LexItemState a)

lexAndYieldLitRationalWithDot :: MonadST.T s m => Lexer s m ()
lexAndYieldLitRationalWithDot = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(c, u) -> go0 c u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState (i0, n0, m0) _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
                n1 = m0 - n0
            LexedToken do
                Token.LitRational if
                    | n1 < 0    -> i1 % 10 ^ negate n1
                    | otherwise -> i1 * 10 ^ n1 % 1
    where
        go0 c u = case lexMaySignUnit u of
            Just lexedSign ->
                (lexedSign, LexItemState (0, 0, 0) do go1)
            Nothing ->
                (LexedSignPositive, go1 (0, 0, 0) c u)

        go1 i@(i0, n0, m0) c u = case u of
            CodeUnit.LcUSymDot ->
                LexItemState i go2
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                LexItemState (i1, n0, m0) go1

        go2 i@(i0, n0, m0) c u = case u of
            CodeUnit.LcUSmallAlphaE ->
                LexItemState i go3
            CodeUnit.LcULargeAlphaE ->
                LexItemState i go3
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                    n1 = n0 + 1
                LexItemState (i1, n1, m0) go2

        go3 i c u = case lexMaySignUnit u of
            Just lexedSign ->
                LexItemState i do go4 lexedSign
            Nothing ->
                go4 LexedSignPositive i c u

        go4 lexedSign (i0, n0, m0) c _ = do
            let m1 = case lexDigitChar c of
                    Nothing ->
                        m0 -- [_]
                    Just n -> case lexedSign of
                        LexedSignPositive -> m0 * 10 + n
                        LexedSignNegative -> m0 * 10 - n
            LexItemState (i0, n0, m1) do go4 lexedSign

lexAndYieldLitRationalWithoutDot :: MonadST.T s m => Lexer s m ()
lexAndYieldLitRationalWithoutDot = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(c, u) -> go0 c u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState (i0, m0) _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
            LexedToken do
                Token.LitRational if
                    | m0 < 0    -> i1 % 10 ^ negate m0
                    | otherwise -> i1 * 10 ^ m0 % 1
    where
        go0 c u = case lexMaySignUnit u of
            Just lexedSign ->
                (lexedSign, LexItemState (0, 0) do go1)
            Nothing ->
                (LexedSignPositive, go1 (0, 0) c u)

        go1 i@(i0, m0) c u = case u of
            CodeUnit.LcUSmallAlphaE ->
                LexItemState i go2
            CodeUnit.LcULargeAlphaE ->
                LexItemState i go2
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                LexItemState (i1, m0) go1

        go2 i c u = case lexMaySignUnit u of
            Just lexedSign ->
                LexItemState i do go3 lexedSign
            Nothing ->
                go3 LexedSignPositive i c u

        go3 lexedSign (i0, m0) c _ = do
            let m1 = case lexDigitChar c of
                    Nothing ->
                        m0 -- [_]
                    Just n -> case lexedSign of
                        LexedSignPositive -> m0 * 10 + n
                        LexedSignNegative -> m0 * 10 - n
            LexItemState (i0, m1) do go3 lexedSign

lexAndYieldLitBitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitBitInteger = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(_, u) -> go0 u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState i0 _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
            LexedToken do Token.LitInteger i1
    where
        go0 u = case lexMaySignUnit u of
            -- rest 0[bB]
            Just lexedSign ->
                (lexedSign, LexItemState 0 do go1 2)
            -- rest [bB]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do go1 1)

        go1 (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 go2
            _ -> LexItemState i0 do go1 do n - 1

        go2 (i0 :: Integer) _ u = do
            let i1 = case u of
                    CodeUnit.LcUNum0 -> i0 * 0b10
                    CodeUnit.LcUNum1 -> i0 * 0b10 + 1
                    _                -> i0 -- [_]
            LexItemState i1 go2

lexAndYieldLitOctitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitOctitInteger = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(_, u) -> go0 u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState i0 _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
            LexedToken do Token.LitInteger i1
    where
        go0 u = case lexMaySignUnit u of
            -- rest 0[oO]
            Just lexedSign ->
                (lexedSign, LexItemState 0 do go1 2)
            -- rest [oO]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do go1 1)

        go1 (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 go2
            _ -> LexItemState i0 do go1 do n - 1

        go2 (i0 :: Integer) _ u = do
            let i1 = case u of
                    CodeUnit.LcUNum0 -> i0 * 0b10
                    CodeUnit.LcUNum1 -> i0 * 0b10 + 1
                    CodeUnit.LcUNum2 -> i0 * 0o10 + 2
                    CodeUnit.LcUNum3 -> i0 * 0o10 + 3
                    CodeUnit.LcUNum4 -> i0 * 0o10 + 4
                    CodeUnit.LcUNum5 -> i0 * 0o10 + 5
                    CodeUnit.LcUNum6 -> i0 * 0o10 + 6
                    CodeUnit.LcUNum7 -> i0 * 0o10 + 7
                    _                -> i0 -- [_]
            LexItemState i1 go2

lexAndYieldLitHexitInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitHexitInteger = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(_, u) -> go0 u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState i0 _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
            LexedToken do Token.LitInteger i1
    where
        go0 u = case lexMaySignUnit u of
            -- rest 0[xX]
            Just lexedSign ->
                (lexedSign, LexItemState 0 do go1 2)
            -- rest [xX]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do go1 1)

        go1 (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 go2
            _ -> LexItemState i0 do go1 do n - 1

        go2 i0 c u = do
            let i1 = case u of
                    CodeUnit.LcUSmallAlphaA -> i0 * 0x10 + 0xA
                    CodeUnit.LcUSmallAlphaB -> i0 * 0x10 + 0xB
                    CodeUnit.LcUSmallAlphaC -> i0 * 0x10 + 0xC
                    CodeUnit.LcUSmallAlphaD -> i0 * 0x10 + 0xD
                    CodeUnit.LcUSmallAlphaE -> i0 * 0x10 + 0xE
                    CodeUnit.LcUSmallAlphaF -> i0 * 0x10 + 0xF
                    CodeUnit.LcULargeAlphaA -> i0 * 0x10 + 0xA
                    CodeUnit.LcULargeAlphaB -> i0 * 0x10 + 0xB
                    CodeUnit.LcULargeAlphaC -> i0 * 0x10 + 0xC
                    CodeUnit.LcULargeAlphaD -> i0 * 0x10 + 0xD
                    CodeUnit.LcULargeAlphaE -> i0 * 0x10 + 0xE
                    CodeUnit.LcULargeAlphaF -> i0 * 0x10 + 0xF
                    _                       -> case lexDigitChar c of
                        Just n  -> i0 * 0x10 + toInteger n
                        Nothing -> i0 -- [_]
            LexItemState i1 go2

lexAndYieldLitDecimalInteger :: MonadST.T s m => Lexer s m ()
lexAndYieldLitDecimalInteger = do
    spgo0 <- consumeBuffer
        do \item -> item <&> \(c, u) -> go0 c u
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        (lexedSign, LexItemState i0 go) -> (lexedSign, go i0 c u)
            }
    lexerYield do
        spgo0 <&> \(lexedSign, LexItemState i0 _) -> do
            let i1 = case lexedSign of
                    LexedSignPositive -> i0
                    LexedSignNegative -> negate i0
            LexedToken do Token.LitInteger i1
    where
        go0 c u = case lexMaySignUnit u of
            Just lexedSign ->
                (lexedSign, LexItemState 0 do go1)
            Nothing  ->
                (LexedSignPositive, go1 0 c u)

        go1 i0 c _ = do
            let i1 = case lexDigitChar c of
                    Just n  -> i0 * 10 + toInteger n
                    Nothing -> i0 -- [_]
            LexItemState i1 go1

data LexedSign
    = LexedSignPositive
    | LexedSignNegative
    deriving (Eq, Ord, Bounded, Enum, Show)

lexMaySignUnit :: CodeUnit.T -> Maybe LexedSign
lexMaySignUnit = \case
    CodeUnit.LcUSymPlus   -> Just LexedSignPositive
    CodeUnit.LcUSymHyphen -> Just LexedSignNegative
    _                     -> Nothing

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

pattern LexEscapeOpen :: CodeUnit.T
pattern LexEscapeOpen = CodeUnit.LcUSymBackslash

lexCharEscByteesc :: MonadST.T s m => Lexer s m (Spanned.Spanned Word8)
lexCharEscByteesc = do
    spgo <- consumeBuffer
        do \item -> item <&> \_ -> LexItemState 0 do go0
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        LexItemState w0 go -> go w0 c u
            }
    pure do spgo <&> \(LexItemState w _) -> w
    where
        go0 i0 _ _ = LexItemState i0 go1

        go1 i0 c u = do
            let i1 = case u of
                    CodeUnit.LcUSmallAlphaA -> i0 * 0x10 + 0xA
                    CodeUnit.LcUSmallAlphaB -> i0 * 0x10 + 0xB
                    CodeUnit.LcUSmallAlphaC -> i0 * 0x10 + 0xC
                    CodeUnit.LcUSmallAlphaD -> i0 * 0x10 + 0xD
                    CodeUnit.LcUSmallAlphaE -> i0 * 0x10 + 0xE
                    CodeUnit.LcUSmallAlphaF -> i0 * 0x10 + 0xF
                    CodeUnit.LcULargeAlphaA -> i0 * 0x10 + 0xA
                    CodeUnit.LcULargeAlphaB -> i0 * 0x10 + 0xB
                    CodeUnit.LcULargeAlphaC -> i0 * 0x10 + 0xC
                    CodeUnit.LcULargeAlphaD -> i0 * 0x10 + 0xD
                    CodeUnit.LcULargeAlphaE -> i0 * 0x10 + 0xE
                    CodeUnit.LcULargeAlphaF -> i0 * 0x10 + 0xF
                    _                       -> case lexDigitChar c of
                        Just n  -> i0 * 0x10 + fromIntegral n
                        Nothing -> error "unreachable: expect a hexit."
            LexItemState i1 go1

graphicWhiteCharCs :: Rules.CharSet
graphicWhiteCharCs = mconcat
    [
        Rules.graphicCs,
        Rules.whiteCharCs
    ]

lexAndYieldLitByteString :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldLitByteString = do
    sp0 <- consumeBufferWithSpan
    go0 sp0 mempty
    where
        go0 sp0 b0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteStringLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymDQuote ->
                        yieldBSBuilder sp1 b0
                    LexEscapeOpen -> do
                        restoreBufferItem item
                        go1 sp1 b0
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        let ci = fromEnum c
                        if
                            | ci >= 0x80 -> do
                                let ci' = ci `mod` 0x100
                                lexerYield do
                                    Spanned.Spanned
                                        {
                                            getSpan = Spanned.getSpan item,
                                            unSpanned = LexError
                                                Error.NonAsciiCharInByteStringLiteral
                                                do text "Found a codepoint higher than 0x7F."
                                        }
                                go0 sp1
                                    do b0 <> BSBuilder.word8 do fromIntegral ci'
                            | otherwise ->
                                go0 sp1
                                    do b0 <> BSBuilder.word8 do fromIntegral ci
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInByteStringLiteral
                                        do text "Found a non graphic char."
                                }
                        go0 sp1 b0

        go1 sp0 b0 = CharEscLex.tlexScan () >>= \case
            Tlex.TlexEndOfInput -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteStringLiteral
                            do text "Found an unclosed literal."
                    }
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithGap -> do
                        sp1 <- consumeBufferWithSpan @s @m
                        go0
                            do sp0 <> sp1
                            do b0
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        go0
                            do sp0 <> sp1
                            do b0 <> BSBuilder.word8 w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        go0
                            do sp0 <> sp1
                            do b0 <> BSBuilder.word8 w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        go0
                            do sp0 <> Spanned.getSpan spw
                            do b0 <> BSBuilder.word8 do Spanned.unSpanned spw
                    Rules.LexUniEscape -> do
                        sp1 <- consumeBufferWithSpan
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexError
                                        Error.UniEscapeInByteStringLiteral
                                        do text "Found an unicode escape in byte literal."
                                }
                        go0
                            do sp0 <> sp1
                            do b0

        yieldBSBuilder sp b = lexerYield do
            let bs = LazyByteString.toStrict do
                    BSBuilder.toLazyByteString b
            Spanned.Spanned
                {
                    getSpan = sp,
                    unSpanned = LexedToken do
                        Token.LitByteString bs
                }

lexAndYieldLitByteChar :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldLitByteChar = do
    sp0 <- consumeBufferWithSpan
    go0 sp0
    where
        go0 sp0 = consumeBufferItem >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteCharLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymQuote -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexError
                                    Error.NoContentByteCharLiteral
                                    do text "Found an literal without contents."
                            }
                    LexEscapeOpen -> do
                        restoreBufferItem item
                        go1 sp1
                    _ | u == CodeUnit.LcUSymSpace ||
                        EnumSet.member u Rules.graphicCs -> do
                        let ci = fromEnum c
                        if
                            | ci >= 0x80 -> do
                                let ci' = ci `mod` 0x100
                                lexerYield do
                                    Spanned.Spanned
                                        {
                                            getSpan = Spanned.getSpan item,
                                            unSpanned = LexError
                                                Error.NonAsciiCharInByteCharLiteral
                                                do text "Found a codepoint higher than 0x7F."
                                        }
                                go3 sp1 do fromIntegral ci'
                            | otherwise ->
                                go2 sp1 do fromIntegral ci
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInByteCharLiteral
                                        do text "Found a non graphic char."
                                }
                        go3 sp1 0x0

        go1 sp0 = CharEscLex.tlexScan () >>= \case
            Tlex.TlexEndOfInput -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteStringLiteral
                            do text "Found an unclosed literal."
                    }
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        go2
                            do sp0 <> sp1
                            w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        go2
                            do sp0 <> sp1
                            w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        go2
                            do sp0 <> Spanned.getSpan spw
                            do Spanned.unSpanned spw
                    Rules.WithGap -> do
                        sp1 <- consumeBufferWithSpan
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexError
                                        Error.GapInByteCharLiteral
                                        do text "Found an gap in byte char literal."
                                }
                        go3
                            do sp0 <> sp1
                            0x0
                    Rules.LexUniEscape -> do
                        sp1 <- consumeBufferWithSpan
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexError
                                        Error.UniEscapeInByteStringLiteral
                                        do text "Found an unicode escape in byte literal."
                                }
                        go3
                            do sp0 <> sp1
                            0x0

        go2 sp0 w = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteCharLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (_, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymQuote -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken do
                                    Token.LitByteChar w
                            }
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.TooManyContentsInByteCharLiteral
                                        do text "Found an excess escape in byte char literal."
                                }
                        go3 sp1 w

        go3 sp0 w = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedByteCharLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (_, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymQuote -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken do
                                    Token.LitByteChar w
                            }
                    _ -> go3 sp1 w

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
