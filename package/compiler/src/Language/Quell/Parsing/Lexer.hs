
{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer (
  lexerConduit,
  lexer,
  LexerMonad (..),
  Lexer (..),
  LexerContext (..),
  LayoutContext (..),
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Lexer.Tlex                as Tlex
import qualified Language.Quell.Parsing.Lexer.Rules as Rules
import qualified Language.Quell.Parsing.Spanned     as Spanned
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Error as Error
import qualified Language.Quell.Type.Token          as Token


$(Rules.buildLexer)

class Monad m => LexerMonad m where
    reportDecodeError :: Text -> Spanned.BytesSpan -> m ()
    reportLexingError :: Text -> Spanned.Span -> Error.T -> m ()

lexerConduit :: forall m. LexerMonad m => Encoding.T
    -> Conduit.ConduitT ByteString Token.T m ()
lexerConduit enc =
    Encoding.decodeConduit enc Conduit..|
    reportDecodeResultConduit Conduit..|
    charLexerConduit
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

charLexerConduit :: LexerMonad m => CharLexerConduitT m ()
charLexerConduit = Conduit.evalStateC
    initialLexerContext
    do unLexer lexer

lexer :: forall m. LexerMonad m => Lexer m ()
lexer = go where
    go :: Lexer m ()
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

newtype Lexer m a = Lexer
    {
        unLexer :: CharLexerConduitT (StateT LexerContext m) a
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via CharLexerConduitT (StateT LexerContext m)

data LexerContext = LexerContext
    {
        currentBuffer          :: Vector (Spanned.T (Char, CodeUnit.T)),
        bufferEndOfSource      :: Bool,
        currentPositionContext :: PositionContext,
        layoutContextStack     :: [LayoutContext],
        lastToken              :: Maybe Token.T,
        lastLoc                :: Spanned.Loc,
        lastLexerState         :: Rules.LexerState
    }
    deriving (Eq, Show)

initialLexerContext :: LexerContext
initialLexerContext = LexerContext
    {
        currentBuffer = mempty,
        currentPositionContext = PositionContext
            {
                bufferPosition = 0,
                locPosition = initialLoc,
                lastChar = Nothing
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

data PositionContext = PositionContext
    {
        bufferPosition :: Int,
        locPosition :: Spanned.Loc,
        lastChar :: Maybe Char
    }
    deriving (Eq, Show)

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

instance Monad m => Tlex.TlexContext PositionContext CodeUnit.T (Lexer m) where
    tlexGetInputPart = Lexer do
        ctx <- Conduit.lift get
        let posCtx = currentPositionContext ctx
        case currentBuffer ctx `index` bufferPosition posCtx of
            Just scu ->
                returnCodeUnit ctx posCtx scu
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
                        loc = locPosition posCtx
                        nlocBytesPos = Spanned.bytesIndex bs + Spanned.bytesLength bs
                        nloc = case undefined Rules.newlineCs of
                            False -> loc
                                {
                                    Spanned.locCol = Spanned.locCol loc + 1,
                                    Spanned.locBytesPos = nlocBytesPos
                                }
                            True  -> case lastChar posCtx of
                                Just '\r' | c == '\n' -> loc
                                    {
                                        Spanned.locBytesPos = nlocBytesPos
                                    }
                                _ -> loc
                                    {
                                        Spanned.locLine = Spanned.locLine loc + 1,
                                        Spanned.locBytesPos = nlocBytesPos
                                    }
                        scu = Spanned.Spanned
                            {
                                Spanned.getSpan = Spanned.Span
                                    {
                                        Spanned.beginLoc = loc,
                                        Spanned.endLoc = nloc
                                    },
                                Spanned.unSpanned = (c, u)
                            }
                        nctx = ctx
                            {
                                currentBuffer = snoc
                                    do currentBuffer ctx
                                    do scu
                            }
                    returnCodeUnit nctx posCtx scu

            returnCodeUnit ctx posCtx scu = do
                let (c, u) = Spanned.unSpanned scu
                    nposCtx = posCtx
                        {
                            bufferPosition = bufferPosition posCtx + 1,
                            locPosition = Spanned.endLoc do Spanned.getSpan scu,
                            lastChar = Just c
                        }
                conduitPut do
                    ctx
                        {
                            currentPositionContext = nposCtx
                        }
                pure do Just u

    tlexGetMark = Lexer do
        ctx <- Conduit.lift get
        pure do currentPositionContext ctx

conduitPut :: Monad m => s -> Conduit.ConduitT i o (StateT s m) ()
conduitPut s = Conduit.lift do put s
