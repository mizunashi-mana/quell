
{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer (
  lexerConduit,
  lexer,
  Lexer (..),
  LexerContext (..),
  LayoutContext (..),
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Lexer.Tlex                as Tlex
import qualified Language.Quell.Parsing.Lexer.Rules as LexerRules
import qualified Language.Quell.Parsing.Spanned     as Spanned
import qualified Language.Quell.Type.Token          as Token


$(LexerRules.buildLexer)

lexerConduit :: Monad m => Conduit.ConduitT ByteString Token.T m ()
lexerConduit = Conduit.evalStateC
  initialLexerContext
  do unLexer lexer

lexer :: forall m. Monad m => Lexer m ()
lexer = go where
  go :: Lexer m ()
  go = do
    ctx <- Lexer do Conduit.lift get
    tlexScan
      do lastLexerState ctx
      >>= \case
        Tlex.TlexEndOfInput ->
          pure ()
        Tlex.TlexError ->
          errorRecoverByTlexScan
        Tlex.TlexAccepted pos act -> undefined pos act

  errorRecoverByTlexScan = undefined

newtype Lexer m a = Lexer
  { unLexer :: Conduit.ConduitT ByteString Token.T (StateT LexerContext m) a
  }
  deriving (
    Functor,
    Applicative,
    Monad
  ) via Conduit.ConduitT ByteString Token.T (StateT LexerContext m)

data LexerContext = LexerContext
  {
    currentBuffer          :: ByteString,
    currentPositionContext :: PositionContext,
    bufferEndOfSource      :: Bool,
    lastToken              :: Maybe Token.T,
    lastLoc                :: Spanned.Loc,
    lastLexerState         :: LexerRules.LexerState,
    layoutContextStack     :: [LayoutContext]
  }
  deriving (Eq, Show)

initialLexerContext :: LexerContext
initialLexerContext = LexerContext
  {
    currentBuffer = mempty,
    currentPositionContext = PositionContext 0,
    bufferEndOfSource = False,
    lastToken = Nothing,
    lastLoc = Spanned.Loc
      {
        Spanned.locLine = 0,
        Spanned.locCol = 0,
        Spanned.locAbsPosition = 0
      },
    lastLexerState = LexerRules.Initial,
    layoutContextStack = []
  }

newtype PositionContext = PositionContext
  {
    bufferPosition :: Int
  }
  deriving (Eq, Show)

data LayoutContext
  = NoLayout
  | CurlyLayout Int
  | DCurlyLayout Int
  deriving (Eq, Show)

instance Monad m => Tlex.TlexContext PositionContext Word8 (Lexer m) where
  tlexGetInputPart = Lexer do
    ctx <- Conduit.lift get
    let oldPos = bufferPosition do currentPositionContext ctx
    let pos = oldPos + 1
    case currentBuffer ctx `index` pos of
      Just w -> do
        Conduit.lift do
          put do ctx { currentPositionContext = PositionContext pos }
        pure do Just w
      Nothing | bufferEndOfSource ctx ->
        pure Nothing
      Nothing -> Conduit.await >>= \case
        Nothing -> do
          Conduit.lift do
            put do ctx { bufferEndOfSource = True }
          pure Nothing
        Just bs -> do
          let newBuf = currentBuffer ctx <> bs
          Conduit.lift do
            put do
              ctx
                {
                  currentBuffer = newBuf,
                  currentPositionContext = PositionContext pos
                }
          case newBuf `index` pos of
            Nothing -> error "unreachable"
            Just w  -> pure do Just w

  tlexGetMark = Lexer do
    ctx <- Conduit.lift get
    pure do currentPositionContext ctx
