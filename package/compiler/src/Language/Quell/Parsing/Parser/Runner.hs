module Language.Quell.Parsing.Parser.Runner (
    T,
    Runner,
    RunnerContext (..),
) where

import Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Parsing.Parser.Layout as Layout


type T = Runner

newtype Runner a = Runner
    {
        unRunner :: Conduit.ConduitT
            (Spanned.T Token.T)
            () -- FIXME
            (State RunnerContext)
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via Conduit.ConduitT
            Token.T
            () -- FIXME
            (State RunnerContext)


data RunnerContext = RunnerContext
    {
        currentLoc :: Spanned.Loc,
        nextToken :: Maybe (Spanned.T Token.T),
        expectNextBrace :: Bool,
        openBraceCont :: Maybe (Int -> Layout.T),
        layoutStack :: Layout.LayoutStack
    }
    deriving (Eq, Show)

initialContext :: RunnerContext
initialContext = RunnerContext
    {
        currentLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            },
        nextToken = Nothing,
        expectNextBrace = False,
        openBrace = Nothing,
        layoutStack = Layout.empty
    }

lexer :: (Spanned.T Token.T -> Runner a) -> Runner a
lexer cont = do
    ctx <- runnerGet
    case openBraceCont ctx of
        Just obCont ->
            openBrace cont obCont
        Nothing ->
            withL cont

pushLayout :: Layout.T -> Runner
pushLayout l = runnerModify' \ctx -> ctx
    {
        layoutStack = Layout.pushLayout l
            do layoutStack ctx
    }

pushVBraceLayout :: Runner
pushVBraceLayout = pushLayout Layout.VBraceLayout

withL :: (Spanned.T Token.T -> Runner a) -> Runner a
withL cont = consumeNextToken >>= \case
    Nothing  ->
        tryEnd cont
    Just spt -> do
        let sp = Spanned.getSpan spt
            bl = Spanned.beginLoc sp
            el = Spanned.endLoc sp
        ctx <- runnerGet
        let cl = currentLoc ctx
            expBrace = case Spanned.unSpanned spt of
                Token.KwWhere   -> True
                Token.KwLet     -> True
                Token.KwLetrec  -> True
                _               -> False
        runnerPut do
            ctx
                {
                    currentLine = el,
                    expectNextBrace = expBrace
                }
        if
            | cl < bl ->
                startNewline cont spt
            | otherwise ->
                cont spt



runnerGet :: Runner RunnerContext
runnerGet = Runner do Conduit.lift do get

runnerPut :: RunnerContext -> Runner ()
runnerPut x = Runner do Conduit.lift do put x

runnerModify' :: (RunnerContext -> RunnerContext) -> Runner ()
runnerModify' = Runner do Conduit.lift do modify'

consumeNextToken :: Runner (Maybe (Spanned.T Token.T))
consumeNextToken = do
    ctx <- runnerGet
    let ctx1 = ctx
            {
                nextToken = Nothing,
                expectNextBrace = False
            }
    mspt <- case nextToken ctx of
        Just x ->
            pure do Just x
        Nothing  ->
            Runner Conduit.await
    case mspt of
        Nothing -> do
            runnerPut ctx1
            if
                | expectNextBrace ctx -> do
                    let cl = currentLoc ctx
                    pure do
                        Spanned.Spanned
                            {
                                Spanned.unSpanned = Token.SpVBraceOpen,
                                Spanned.getSpan = Spanned.Span
                                    {
                                        Spanned.beginLoc = cl,
                                        Spanned.endLoc = cl
                                    }
                            }
                | otherwise ->
                    pure Nothing
        Just spt -> do
            let (isBrace, expBrace) = case Spanned.unSpanned spt of
                    Token.KwWhere       -> (False,  True)
                    Token.KwLet         -> (False,  True)
                    Token.KwLetrec      -> (False,  True)
                    Token.SpBraceOpen   -> (True,   False)
                    Token.SpDBraceOpen  -> (True,   False)
                    Token.SpVBraceOpen  -> (True,   False)
                    _                   -> (False,  False)
                ctx2 = ctx1
                    {
                        expectNextBrace = expBrace
                    }
            if
                | expectNextBrace ctx && not isBrace -> do
                    let cl = currentLoc ctx
                    runnerPut do ctx2 { nextToken = Just spt }
                    pure do
                        Just do
                            Spanned.Spanned
                                {
                                    Spanned.getSpan = Spanned.Span
                                        {
                                            Spanned.beginLoc = cl,
                                            Spanned.endLoc = cl
                                        },
                                    Spanned.unSpanned = Token.SpVBraceOpen
                                }
                | otherwise -> do
                    runnerPut ctx2
                    pure do Just spt

