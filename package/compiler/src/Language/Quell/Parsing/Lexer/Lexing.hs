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
                yieldError
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
                    Spanned.getSpan = sp,
                    Spanned.unSpanned = (c, u)
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

newEmptyBuffer :: ST s (STBuffer.T s (Spanned.T (Char, CodeUnit.T)))
newEmptyBuffer = STBuffer.new []

lexerGet :: Monad m => Lexer s m (LexerContext s)
lexerGet = Lexer do Conduit.lift get

lexerModify :: Monad m => (LexerContext s -> LexerContext s) -> Lexer s m ()
lexerModify f = Lexer do Conduit.lift do modify' f

lexerPut :: Monad m => LexerContext s -> Lexer s m ()
lexerPut ctx = Lexer do Conduit.lift do put ctx

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

-- FIXME: try error recovering and report detailed and suggestions
yieldError :: MonadST.T s m => Lexer s m ()
yieldError = Lexer do
    Conduit.yield do undefined do LexError Error.UnexpectedCodeUnits do text "TODO"

yieldToken :: MonadST.T s m => Token.T -> Lexer s m ()
yieldToken t = do
    sp0 <- consumeBuffer
        do \item ->
            Spanned.getSpan item
        do \sp item ->
            sp <> Spanned.getSpan item
    let u = Spanned.Spanned
            {
                Spanned.getSpan = sp0,
                Spanned.unSpanned = LexedToken t
            }
    Lexer do Conduit.yield u

yieldIdToken :: MonadST.T s m => (TextId.T -> Token.T) -> Lexer s m ()
yieldIdToken t = do
    sptxtB0 <- consumeBuffer
        do \item -> item <&> \(c, _) -> textBuilderFromChar c
        do \sptxtB item -> do
            let (c, _) = Spanned.unSpanned item
            Spanned.Spanned
                {
                    Spanned.getSpan =
                        Spanned.getSpan sptxtB <> Spanned.getSpan item,
                    Spanned.unSpanned =
                        Spanned.unSpanned sptxtB <> textBuilderFromChar c
                }
    let u = sptxtB0 <&> \txtB0 ->
            LexedToken do t do TextId.textId do buildStrictText txtB0
    Lexer do Conduit.yield u

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
    go
        do Spanned.unSpanned spIsNegate
        do Spanned.getSpan spIsNegate
        do 0
    where
        go isN sp i0 = consumeBufferItem >>= \case
            Nothing -> do
                let i1 = case isN of
                        True  -> negate i0
                        False -> i0
                    u = Spanned.Spanned
                        {
                            getSpan = sp,
                            unSpanned = LexedToken do Token.LitInteger i1
                        }
                Lexer do Conduit.yield u
            Just item -> undefined item

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
