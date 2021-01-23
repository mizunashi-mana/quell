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
            LexedToken do t do TextId.textId do buildText txtB0
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
                (lexedSign, LexItemState (0, 0, 0) do goDecimal1)
            Nothing ->
                (LexedSignPositive, goDecimal1 (0, 0, 0) c u)

        goDecimal1 i@(i0, n0, m0) c u = case u of
            CodeUnit.LcUSymDot ->
                LexItemState i goDecimal2
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                LexItemState (i1, n0, m0) goDecimal1

        goDecimal2 i@(i0, n0, m0) c u = case u of
            CodeUnit.LcUSmallAlphaE ->
                LexItemState i goExponent
            CodeUnit.LcULargeAlphaE ->
                LexItemState i goExponent
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                    n1 = n0 + 1
                LexItemState (i1, n1, m0) goDecimal2

        goExponent i c u = case lexMaySignUnit u of
            Just lexedSign ->
                LexItemState i do goExpDecimal lexedSign
            Nothing ->
                goExpDecimal LexedSignPositive i c u

        goExpDecimal lexedSign (i0, n0, m0) c _ = do
            let m1 = case lexDigitChar c of
                    Nothing ->
                        m0 -- [_]
                    Just n -> case lexedSign of
                        LexedSignPositive -> m0 * 10 + n
                        LexedSignNegative -> m0 * 10 - n
            LexItemState (i0, n0, m1) do goExpDecimal lexedSign

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
                (lexedSign, LexItemState (0, 0) do goDecimal)
            Nothing ->
                (LexedSignPositive, goDecimal (0, 0) c u)

        goDecimal i@(i0, m0) c u = case u of
            CodeUnit.LcUSmallAlphaE ->
                LexItemState i goExponent
            CodeUnit.LcULargeAlphaE ->
                LexItemState i goExponent
            _ -> do
                let i1 = case lexDigitChar c of
                        Just n  -> i0 * 10 + toInteger n
                        Nothing -> i0 -- [_]
                LexItemState (i1, m0) goDecimal

        goExponent i c u = case lexMaySignUnit u of
            Just lexedSign ->
                LexItemState i do goExpDecimal lexedSign
            Nothing ->
                goExpDecimal LexedSignPositive i c u

        goExpDecimal lexedSign (i0, m0) c _ = do
            let m1 = case lexDigitChar c of
                    Nothing ->
                        m0 -- [_]
                    Just n -> case lexedSign of
                        LexedSignPositive -> m0 * 10 + n
                        LexedSignNegative -> m0 * 10 - n
            LexItemState (i0, m1) do goExpDecimal lexedSign

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
                (lexedSign, LexItemState 0 do goSkipPref 2)
            -- rest [bB]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do goSkipPref 1)

        goSkipPref (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 goBit_
            _ -> LexItemState i0 do goSkipPref do n - 1

        goBit_ (i0 :: Integer) _ u = do
            let i1 = case u of
                    CodeUnit.LcUNum0 -> i0 * 0b10
                    CodeUnit.LcUNum1 -> i0 * 0b10 + 1
                    _                -> i0 -- [_]
            LexItemState i1 goBit_

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
                (lexedSign, LexItemState 0 do goSkipPref 2)
            -- rest [oO]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do goSkipPref 1)

        goSkipPref (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 goOctet_
            _ -> LexItemState i0 do goSkipPref do n - 1

        goOctet_ (i0 :: Integer) _ u = do
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
            LexItemState i1 goOctet_

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
                (lexedSign, LexItemState 0 do goSkipPref 2)
            -- rest [xX]
            Nothing  ->
                (LexedSignPositive, LexItemState 0 do goSkipPref 1)

        goSkipPref (n :: Int) i0 _ _ = case n of
            1 -> LexItemState i0 goHexit_
            _ -> LexItemState i0 do goSkipPref do n - 1

        goHexit_ i0 c u = do
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
            LexItemState i1 goHexit_

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
                (lexedSign, LexItemState 0 do goDecimal)
            Nothing  ->
                (LexedSignPositive, goDecimal 0 c u)

        goDecimal i0 c _ = do
            let i1 = case lexDigitChar c of
                    Just n  -> i0 * 10 + toInteger n
                    Nothing -> i0 -- [_]
            LexItemState i1 goDecimal

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
{-# INLINE lexDigitChar #-}
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
        do \item -> item <&> \_ -> LexItemState 0 do goHexit
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        LexItemState w0 go -> go w0 c u
            }
    pure do spgo <&> \(LexItemState w _) -> w
    where
        goHexit i0 c u = do
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
            LexItemState i1 goHexit

lexCharEscUniEscape :: MonadST.T s m => Lexer s m (Spanned.Spanned Char)
lexCharEscUniEscape = do
    spgo <- consumeBuffer
        do \item -> item <&> \_ -> LexItemState 0 do go0
        do \spgo item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spgo <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, u) -> case Spanned.unSpanned spgo of
                        LexItemState i0 go -> go i0 c u
            }
    pure do spgo <&> \(LexItemState i _) -> toEnum i
    where
        go0 i0 _ _ = LexItemState i0 goHexit

        goHexit i0 c u = do
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
                        Nothing -> i0 -- [}]
            LexItemState i1 goHexit

graphicWhiteCharCs :: Rules.CharSet
graphicWhiteCharCs = mconcat
    [
        Rules.graphicCs,
        Rules.whiteCharCs
    ]

lexAndYieldLitByteString :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldLitByteString = do
    sp0 <- consumeBufferWithSpan
    goByteChar sp0 mempty
    where
        goByteChar sp0 b0 = consumeBufferItem @s @m >>= \case
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
                    LexEscapeOpen ->
                        goEscape sp1 b0
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
                                goByteChar sp1
                                    do b0 <> BSBuilder.word8 do fromIntegral ci'
                            | otherwise ->
                                goByteChar sp1
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
                        goByteChar sp1 b0

        goEscape sp0 b0 = CharEscLex.tlexScan () >>= \case
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
                        goByteChar
                            do sp0 <> sp1
                            do b0
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        goByteChar
                            do sp0 <> sp1
                            do b0 <> BSBuilder.word8 w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        goByteChar
                            do sp0 <> sp1
                            do b0 <> BSBuilder.word8 w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        goByteChar
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
                        goByteChar
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
    goByteChar sp0
    where
        goByteChar sp0 = consumeBufferItem >>= \case
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
                    LexEscapeOpen ->
                        goEscape sp1
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
                                goClose sp1 do fromIntegral ci'
                            | otherwise ->
                                goClose sp1 do fromIntegral ci
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInByteCharLiteral
                                        do text "Found a non graphic char."
                                }
                        goTooMany sp1 0x0

        goEscape sp0 = CharEscLex.tlexScan () >>= \case
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
                        goClose
                            do sp0 <> sp1
                            w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        goClose
                            do sp0 <> sp1
                            w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        goClose
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
                        goTooMany
                            do sp0 <> sp1
                            0x0
                    Rules.LexUniEscape -> do
                        sp1 <- consumeBufferWithSpan
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexError
                                        Error.UniEscapeInByteCharLiteral
                                        do text "Found an unicode escape in byte literal."
                                }
                        goTooMany
                            do sp0 <> sp1
                            0x0

        goClose sp0 w = consumeBufferItem @s @m >>= \case
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
                        goTooMany sp1 w

        goTooMany sp0 w = consumeBufferItem @s @m >>= \case
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
                    _ -> goTooMany sp1 w

lexAndYieldLitString :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldLitString = do
    sp0 <- consumeBufferWithSpan
    goChar sp0 mempty
    where
        goChar sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedStringLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymDQuote ->
                        yieldTextBuilder sp1 t0
                    LexEscapeOpen ->
                        goEscape sp1 t0
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        goChar sp1
                            do t0 <> textBuilderFromChar c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInStringLiteral
                                        do text "Found a non graphic char."
                                }
                        goChar sp1 t0

        goEscape sp0 t0 = CharEscLex.tlexScan () >>= \case
            Tlex.TlexEndOfInput -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedStringLiteral
                            do text "Found an unclosed literal."
                    }
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithGap -> do
                        sp1 <- consumeBufferWithSpan @s @m
                        goChar
                            do sp0 <> sp1
                            do t0
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        goChar
                            do sp0 <> sp1
                            do t0 <> textBuilderFromWord8 w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        goChar
                            do sp0 <> sp1
                            do t0 <> textBuilderFromWord8 w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        let w = Spanned.unSpanned spw
                        goChar
                            do sp0 <> Spanned.getSpan spw
                            do t0 <> textBuilderFromWord8 w
                    Rules.LexUniEscape -> do
                        spc <- lexCharEscUniEscape
                        let c = Spanned.unSpanned spc
                        goChar
                            do sp0 <> Spanned.getSpan spc
                            do t0 <> textBuilderFromChar c

        yieldTextBuilder sp tb = lexerYield do
            Spanned.Spanned
                {
                    getSpan = sp,
                    unSpanned = LexedToken do
                        Token.LitString do buildText tb
                }

        textBuilderFromWord8 w = textBuilderFromChar
            do toEnum do fromIntegral w

lexAndYieldLitChar :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldLitChar = do
    sp0 <- consumeBufferWithSpan
    goChar sp0
    where
        goChar sp0 = consumeBufferItem >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedCharLiteral
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
                                    Error.NoContentCharLiteral
                                    do text "Found an literal without contents."
                            }
                    LexEscapeOpen ->
                        goEscape sp1
                    _ | u == CodeUnit.LcUSymSpace ||
                        EnumSet.member u Rules.graphicCs -> do
                        goClose sp1 c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInCharLiteral
                                        do text "Found a non graphic char."
                                }
                        goTooMany sp1 '\NUL'

        goEscape sp0 = CharEscLex.tlexScan () >>= \case
            Tlex.TlexEndOfInput -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedCharLiteral
                            do text "Found an unclosed literal."
                    }
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        let c = toEnum do fromIntegral w
                        goClose
                            do sp0 <> sp1
                            c
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        let c = toEnum do fromIntegral w
                        goClose
                            do sp0 <> sp1
                            c
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        let c = toEnum
                                do fromIntegral do Spanned.unSpanned spw
                        goClose
                            do sp0 <> Spanned.getSpan spw
                            c
                    Rules.WithGap -> do
                        sp1 <- consumeBufferWithSpan
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexError
                                        Error.GapInCharLiteral
                                        do text "Found an gap in char literal."
                                }
                        goTooMany
                            do sp0 <> sp1
                            '\NUL'
                    Rules.LexUniEscape -> do
                        spc <- lexCharEscUniEscape
                        goClose
                            do sp0 <> Spanned.getSpan spc
                            do Spanned.unSpanned spc

        goClose sp0 c = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedCharLiteral
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
                                    Token.LitChar c
                            }
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.TooManyContentsInCharLiteral
                                        do text "Found an excess escape in char literal."
                                }
                        goTooMany sp1 c

        goTooMany sp0 c = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedCharLiteral
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
                                    Token.LitChar c
                            }
                    _ -> goTooMany sp1 c

lexAndYieldInterpStringStart :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldInterpStringStart = do
    sp0 <- consumeBufferWithSpan
    lexAndYieldInterpString True sp0

lexAndYieldInterpStringContinue :: MonadST.T s m => Lexer s m ()
lexAndYieldInterpStringContinue = do
    sp0 <- consumeBufferWithSpan
    lexAndYieldInterpString False sp0

lexAndYieldInterpString :: forall s m. MonadST.T s m
    => Bool -> Spanned.Span -> Lexer s m ()
lexAndYieldInterpString b = \sp0 -> goChar sp0 mempty
    where
        goChar sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedInterpStringLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymDQuote -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken case b of
                                    True ->
                                        Token.LitInterpStringWithoutInterp
                                            do buildText t0
                                    False ->
                                        Token.LitInterpStringEnd
                                            do buildText t0
                            }
                    CodeUnit.LcUSymDollar ->
                        goInterpOpen sp1 t0
                            do Spanned.getSpan item
                    LexEscapeOpen ->
                        goEscape sp1 t0
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        goChar sp1
                            do t0 <> textBuilderFromChar c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInInterpStringLiteral
                                        do text "Found a non graphic char."
                                }
                        goChar sp1 t0

        goEscape sp0 t0 = CharEscLex.tlexScan () >>= \case
            Tlex.TlexEndOfInput -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedInterpStringLiteral
                            do text "Found an unclosed literal."
                    }
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos act -> do
                setPosition pos
                case act of
                    Rules.WithGap -> do
                        sp1 <- consumeBufferWithSpan @s @m
                        goChar
                            do sp0 <> sp1
                            do t0
                    Rules.WithCharesc w -> do
                        sp1 <- consumeBufferWithSpan
                        goChar
                            do sp0 <> sp1
                            do t0 <> textBuilderFromWord8 w
                    Rules.WithAsciiEsc w -> do
                        sp1 <- consumeBufferWithSpan
                        goChar
                            do sp0 <> sp1
                            do t0 <> textBuilderFromWord8 w
                    Rules.LexByteesc -> do
                        spw <- lexCharEscByteesc
                        let w = Spanned.unSpanned spw
                        goChar
                            do sp0 <> Spanned.getSpan spw
                            do t0 <> textBuilderFromWord8 w
                    Rules.LexUniEscape -> do
                        spc <- lexCharEscUniEscape
                        let c = Spanned.unSpanned spc
                        goChar
                            do sp0 <> Spanned.getSpan spc
                            do t0 <> textBuilderFromChar c

        goInterpOpen sp0 t0 isp0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedInterpStringLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                    isp1 = isp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymBraceOpen ->
                        goInterpBraceOpen sp1 t0 isp1
                    CodeUnit.LcUSymWhiteBraceOpen -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken case b of
                                    True ->
                                        Token.LitInterpStringStart
                                            do buildText t0
                                    False ->
                                        Token.LitInterpStringContinue
                                            do buildText t0
                            }
                    _ -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = isp1,
                                unSpanned = LexError
                                    Error.InvalidInterpOpenInInterpStringLiteral
                                    do text "Unexpected a interp opening."
                            }

        goInterpBraceOpen sp0 t0 isp0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedInterpStringLiteral
                            do text "Found an unclosed literal."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                    isp1 = isp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymHash -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken case b of
                                    True ->
                                        Token.LitInterpStringStart
                                            do buildText t0
                                    False ->
                                        Token.LitInterpStringContinue
                                            do buildText t0
                            }
                    _ -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = isp1,
                                unSpanned = LexError
                                    Error.InvalidInterpOpenInInterpStringLiteral
                                    do text "Unexpected a interp opening."
                            }

        textBuilderFromWord8 w = textBuilderFromChar
            do toEnum do fromIntegral w

graphicSpaceCs :: Rules.CharSet
graphicSpaceCs = mconcat
    [
        Rules.graphicCs,
        Rules.spaceCs
    ]

lexAndYieldCommentLineWithContent :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldCommentLineWithContent = do
    spc <- consumeBuffer
        do \item -> item <&> \(c, _) -> c
        do \spc item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spc <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, _) -> c
            }
    goChar
        do Spanned.getSpan spc
        do textBuilderFromChar do Spanned.unSpanned spc
    where
        goChar sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexedToken do
                            Token.CommentLine
                                do buildText t0
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymCR -> goLF sp1 t0
                    _ | EnumSet.member u Rules.newlineCs -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken do
                                    Token.CommentLine
                                        do buildText t0
                            }
                    _ | EnumSet.member u graphicSpaceCs -> do
                        goChar sp1
                            do t0 <> textBuilderFromChar c
                    _ -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = Spanned.getSpan item,
                                unSpanned = LexError
                                    Error.NonGraphicInLineComment
                                    do text "Non graphic char in line comment."
                            }

        goLF sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexedToken do
                            Token.CommentLine
                                do buildText t0
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymLF -> lexerYield do
                        Spanned.Spanned
                            {
                                getSpan = sp1,
                                unSpanned = LexedToken do
                                    Token.CommentLine
                                        do buildText t0
                            }
                    _ -> do
                        restoreBufferItem item
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp0,
                                    unSpanned = LexedToken do
                                        Token.CommentLine
                                            do buildText t0
                                }

lexAndYieldCommentMultilineWithContent :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldCommentMultilineWithContent = do
    spc <- consumeBuffer
        do \item -> item <&> \(c, _) -> c
        do \spc item -> Spanned.Spanned
            {
                getSpan = Spanned.getSpan spc <> Spanned.getSpan item,
                unSpanned = case Spanned.unSpanned item of
                    (c, _) -> c
            }
    goChar False 0
        do Spanned.getSpan spc
        do textBuilderFromChar do Spanned.unSpanned spc
    where
        goChar b i sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedMultilineComment
                            do text "Unclosed comment."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                    t1 = case b of
                        True -> t0 <> textBuilderFromChar '-'
                        False -> t0
                case u of
                    CodeUnit.LcUSymBraceOpen ->
                        goChar False
                            do i + 1
                            sp1
                            do t1 <> textBuilderFromChar c
                    CodeUnit.LcUSymHyphen ->
                        goChar True i sp1
                            do t1 <> textBuilderFromChar c
                    CodeUnit.LcUSymBraceClose | b -> if
                        | i > 0 ->
                            goChar False
                                do i - 1
                                sp1
                                do t1 <> textBuilderFromChar c
                        | otherwise -> lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = sp1,
                                    unSpanned = LexedToken do
                                        Token.CommentMultiline
                                            do buildText t0
                                }
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        goChar False i sp1
                            do t1 <> textBuilderFromChar c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInMultilineComment
                                        do text "Non graphic char in multi line comment."
                                }
                        goChar False i sp1 t1

lexAndYieldCommentDoc :: forall s m. MonadST.T s m => Lexer s m ()
lexAndYieldCommentDoc = do
    sp0 <- consumeBufferWithSpan
    goChar sp0 mempty
    where
        goChar sp0 t0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedDocComment
                            do text "Unclosed comment."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    CodeUnit.LcUSymCR ->
                        goClose True sp1 initRestUnits t0
                            do textBuilderFromChar c
                    _ | EnumSet.member u Rules.newlineCs ->
                        goClose False sp1 initRestUnits t0
                            do textBuilderFromChar c
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        goChar sp1
                            do t0 <> textBuilderFromChar c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInDocComment
                                        do text "Non graphic char in doc comment."
                                }
                        goChar sp1 t0

        goClose b sp0 rs t0 rt0 = case rs of
            [] -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexedToken do
                            Token.CommentDoc do buildText t0
                    }
            r:rs -> goClose b sp0 r rs t0 rt0

        goClose1 b sp0 r rs t0 rt0 = consumeBufferItem @s @m >>= \case
            Nothing -> lexerYield do
                Spanned.Spanned
                    {
                        getSpan = sp0,
                        unSpanned = LexError
                            Error.UnclosedDocComment
                            do text "Unclosed comment."
                    }
            Just item -> do
                let (c, u) = Spanned.unSpanned item
                    sp1 = sp0 <> Spanned.getSpan item
                case u of
                    _ | r == u ->
                        goClose False sp1 rs t0
                            do rt0 <> textBuilderFromChar c
                    CodeUnit.LcUSymLF | b ->
                        goClose False sp1 initRestUnits t0
                            do rt0 <> textBuilderFromChar c
                    CodeUnit.LcUSymCR ->
                        goClose True sp1 initRestUnits
                            do t0 <> rt0
                            do textBuilderFromChar c
                    _ | EnumSet.member u Rules.newlineCs ->
                        goClose False sp1 initRestUnits
                            do t0 <> rt0
                            do textBuilderFromChar c
                    _ | EnumSet.member u graphicWhiteCharCs -> do
                        goChar sp1
                            do t0 <> rt0 <> textBuilderFromChar c
                    _ -> do
                        lexerYield do
                            Spanned.Spanned
                                {
                                    getSpan = Spanned.getSpan item,
                                    unSpanned = LexError
                                        Error.NonGraphicInDocComment
                                        do text "Non graphic char in doc comment."
                                }
                        goChar sp1 do t0 <> rt0

        initRestUnits =
            [
                CodeUnit.LcUSymVertBar,
                CodeUnit.LcUSymHyphen,
                CodeUnit.LcUSymBraceClose
            ]

lexAndYieldCommentPragma :: MonadST.T s m => Lexer s m ()
lexAndYieldCommentPragma = error "TODO"
