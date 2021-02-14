module Language.Quell.Parsing.Parser.Runner (
    T,
    Runner,
    RunnerContext (..),
    runRunner,
    lexer,
    errorRecover,
) where

import Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Data.Bag as Bag
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Parsing.Parser.Error as Error
import qualified Language.Quell.Parsing.Parser.Layout as Layout


type T = Runner

type RunnerConduitT = Conduit.ConduitT
    Layout.TokenWithL
    Conduit.Void

newtype Runner m a = Runner
    (
        RunnerConduitT
            (StateT (RunnerContext m) m)
            a
    )
    deriving (
        Functor,
        Applicative,
        Monad
    ) via RunnerConduitT
        (StateT (RunnerContext m) m)

runRunner :: Monad m => Runner m a -> RunnerConduitT m a
runRunner = \case
    Runner m -> Conduit.evalStateC initialContext m

type RunnerCont m a = Spanned.T Token.T -> Runner m a

data RunnerContext m = RunnerContext
    {
        withLCont :: forall a. RunnerCont m a -> Runner m a,
        lastSpan :: Spanned.Span,
        tokenStack :: [Layout.TokenWithL],
        parseErrors :: Bag.T Error.T
    }

initialContext :: Monad m => RunnerContext m
initialContext = RunnerContext
    {
        withLCont = \p -> withL p False [],
        lastSpan = Spanned.Span
            {
                Spanned.beginLoc = lastLoc,
                Spanned.endLoc = lastLoc
            },
        tokenStack = [],
        parseErrors = mempty
    }
    where
        lastLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            }

lexer :: Monad m => RunnerCont m a -> Runner m a
lexer p0 = do
    ctx0 <- runnerGet
    withLCont ctx0 p0

withL :: Monad m => RunnerCont m a -> Bool -> [Layout.T] -> Runner m a
withL p0 expB ms = consumeToken >>= \case
    Nothing -> do
        ctx0 <- runnerGet
        let lc = Spanned.endLoc do lastSpan ctx0
        resolveEmptyBrace p0 expB lc \p1 ->
            tryEnd p1 ms
    Just twl -> case twl of
        Layout.Token isN spt
            | isN ->
                resolveNewline p0 spt expB ms
            | otherwise ->
                resolveToken p0 spt expB ms
        Layout.ExpectBrace ->
            withL p0 True ms

errorRecover :: Runner m a
errorRecover = undefined

resolveToken :: Monad m => RunnerCont m a
    -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner m a
resolveToken p0 spt expB ms = do
    case Spanned.unSpanned spt of
        Token.SpBraceOpen | expB ->
            runParserL p0 spt \p1 ->
                withL p1 False do Layout.ExplicitBrace:ms
        Token.SpDBraceOpen | expB ->
            runParserL p0 spt \p1 -> do
                m <- calcLayoutPos
                withL p1 False do Layout.ExplicitDBrace m:ms
        _ | expB -> do
            let vbOp = Spanned.spannedFromLoc
                    do Spanned.beginLoc do Spanned.getSpan spt
                    do Token.SpVBraceOpen
            runParserL p0 vbOp \p1 -> do
                m <- calcLayoutPos
                resolveToken p1 spt False do Layout.VirtualBrace m:ms
        t | Layout.isOpen t ->
            runParserL p0 spt \p1 -> do
                withL p1 False do Layout.NoLayout:ms
        t | Layout.isClose t ->
            tryClose p0 spt ms
        Token.LitInterpStringContinue{} ->
            tryClose p0 spt ms
        _ ->
            runParserL p0 spt \p1 -> do
                withL p1 False ms

resolveNewline :: Monad m
    => RunnerCont m a -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner m a
resolveNewline p0 spt expB ms0 = do
    let bl = Spanned.beginLoc do Spanned.getSpan spt
        c = Spanned.locCol bl
    case ms0 of
        Layout.ExplicitDBrace m:ms1
            | c < m -> case Spanned.unSpanned spt of
                Token.SpDBraceClose ->
                    resolveEmptyBrace p0 expB bl \p1 ->
                        runParserL p1 spt \p2 ->
                            withL p2 False ms1
                _ -> error "parse error"
            | c == m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL p1 vsemi \p2 ->
                        resolveToken p2 spt False ms0
            | otherwise ->
                resolveToken p0 spt False ms0
        Layout.VirtualBrace m:ms1
            | c < m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vbCl = Spanned.spannedFromLoc bl
                            Token.SpVBraceClose
                    runParserL p1 vbCl \p2 ->
                        resolveNewline p2 spt False ms1
            | c == m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL p1 vsemi \p2 ->
                        resolveToken p2 spt False ms0
            | otherwise ->
                resolveToken p0 spt expB ms0
        _ ->
            resolveToken p0 spt expB ms0

resolveEmptyBrace :: Monad m
    => RunnerCont m a -> Bool -> Spanned.Loc
    -> (forall b. RunnerCont m b -> Runner m b) -> Runner m a
resolveEmptyBrace p0 expB lc cont = case expB of
    False ->
        cont p0
    True -> do
        let vbOp = Spanned.spannedFromLoc lc
                Token.SpVBraceOpen
        runParserL p0 vbOp \p1 -> do
            let vbCl = Spanned.spannedFromLoc lc
                    Token.SpVBraceClose
            runParserL p1 vbCl \p2 ->
                cont p2

tryClose :: Monad m
    => RunnerCont m a -> Spanned.T Token.T -> [Layout.T] -> Runner m a
tryClose p0 spt ms0 = case ms0 of
    [] ->
        error "parse error"
    m:ms1 -> case m of
        Layout.VirtualBrace _ -> do
            let lc = Spanned.beginLoc do Spanned.getSpan spt
                vbCl = Spanned.spannedFromLoc lc
                    Token.SpVBraceClose
            runParserL p0 vbCl \p1 ->
                tryClose p1 spt ms1
        Layout.ExplicitBrace -> case Spanned.unSpanned spt of
            Token.SpBraceOpen -> runParserL p0 spt \p1 ->
                withL p1 False ms1
            _ ->
                error "parse error"
        Layout.ExplicitDBrace _ -> case Spanned.unSpanned spt of
            Token.SpDBraceOpen -> runParserL p0 spt \p1 ->
                withL p1 False ms1
            _ ->
                error "parse error"
        Layout.NoLayout -> case Spanned.unSpanned spt of
            Token.LitInterpStringContinue{} -> runParserL p0 spt \p1 ->
                withL p1 False do Layout.NoLayout:ms1
            _ -> runParserL p0 spt \p1 ->
                withL p1 False ms1

tryEnd :: Monad m => RunnerCont m a -> [Layout.T] -> Runner m a
tryEnd = \p0 ms0 -> do
    ctx0 <- runnerGet
    let lc = Spanned.endLoc do lastSpan ctx0
    go lc p0 ms0
    where
        go :: Monad m
            => Spanned.Loc -> RunnerCont m b -> [Layout.T] -> Runner m b
        go lc p0 ms0 = case ms0 of
            [] -> do
                let eos = Spanned.spannedFromLoc lc
                        Token.EndOfSource
                runParserL p0 eos \_ ->
                    error "unreachable"
            m:ms1 -> case m of
                Layout.VirtualBrace _ -> do
                    let vbOp = Spanned.spannedFromLoc lc
                            Token.SpVBraceOpen
                    runParserL p0 vbOp \p1 ->
                        go lc p1 ms1
                _ ->
                    error "parse error"


runParserL :: Monad m
    => RunnerCont m a -> Spanned.T Token.T
    -> (forall b. RunnerCont m b -> Runner m b) -> Runner m a
runParserL p spt cont = do
    runnerModify' \ctx -> ctx
        {
            withLCont = cont
        }
    p spt

calcLayoutPos :: forall m. Monad m => Runner m Int
calcLayoutPos = do
    ctx0 <- runnerGet
    go0 do tokenStack ctx0
    where
        go0 :: [Layout.TokenWithL] -> Runner m Int
        go0 = \case
            Layout.Token _ spt:_ -> do
                let sp = Spanned.getSpan spt
                    c = Spanned.locCol do Spanned.beginLoc sp
                pure c
            Layout.ExpectBrace:ts ->
                go0 ts
            [] ->
                go1 []

        go1 :: [Layout.TokenWithL] -> Runner m Int
        go1 ts0 = Runner Conduit.await >>= \case
            Nothing -> do
                restoreTokenStack ts0
                pure 0
            Just t -> do
                case t of
                    Layout.Token _ spt -> do
                        let sp = Spanned.getSpan spt
                            c = Spanned.locCol do Spanned.beginLoc sp
                        restoreTokenStack do t:ts0
                        pure c
                    Layout.ExpectBrace -> do
                        go1 do t:ts0

        restoreTokenStack ts = runnerModify' \ctx -> ctx
            {
                -- expect token stack having few elements
                tokenStack = tokenStack ctx ++ reverse ts
            }

consumeToken :: Monad m => Runner m (Maybe Layout.TokenWithL)
consumeToken = do
    ctx0 <- runnerGet
    mt <- case tokenStack ctx0 of
        [] ->
            Runner Conduit.await
        t:ts -> do
            runnerPut do
                ctx0
                    {
                        tokenStack = ts
                    }
            pure do Just t
    ctx1 <- runnerGet
    case mt of
        Just (Layout.Token _ spt) -> do
            runnerPut do
                ctx1
                    {
                        lastSpan = Spanned.getSpan spt
                    }
        _ ->
            pure ()
    pure mt

runnerGet :: Monad m => Runner m (RunnerContext m)
runnerGet = Runner do Conduit.lift get

runnerPut :: Monad m => RunnerContext m -> Runner m ()
runnerPut x = Runner do Conduit.lift do put x

runnerModify' :: Monad m => (RunnerContext m -> RunnerContext m) -> Runner m ()
runnerModify' f = Runner do Conduit.lift do modify' f
