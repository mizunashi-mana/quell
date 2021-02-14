module Language.Quell.Parsing.Parser.Runner (
    T,
    Runner,
    RunnerContext (..),
) where

import Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Data.Bag as Bag
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Parsing.Parser.Error as Error
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Parsing.Parser.PreParse as PreParse


type T = Runner

newtype Runner a = Runner
    {
        unRunner :: Conduit.ConduitT
            Layout.TokenWithL Void
            (StateT RunnerContext Maybe)
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via Conduit.ConduitT
            Layout.TokenWithL Void
            (StateT RunnerContext Maybe)


type RunnerCont a = Spanned.T Token.T -> Runner a

data RunnerContext = RunnerContext
    {
        withLCont :: forall a. RunnerCont a -> Runner a,
        lastSpan :: Spanned.Span,
        tokenStack :: [Layout.TokenWithL],
        parseErrors :: Bag.T Error.T
    }

initialContext :: RunnerContext
initialContext = RunnerContext
    {
        withLCont = \cont -> withL cont False [],
        lastSpan = Spanned.Span
            {
                Spanned.beginLoc = lastLoc,
                Spanned.endLoc = lastLoc
            },
        expectBrace = False,
        tokenStack = [],
        layoutStack = [],
        parseErrors = mempty
    }
    where
        lastLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            }

lexer :: RunnerCont a -> Runner a
lexer cont0 = do
    ctx0 <- runnerGet
    withLCont ctx0 cont0

withL :: RunnerCont a -> Bool -> [Layout.T] -> Runner a
withL cont0 expB ms = consumeToken >>= \case
    Nothing -> resolveEmptyBrace cont0 expB \cont1 ->
        tryEnd cont1 ms
    Just twl -> case twl of
        Layout.Token isN spt
            | isN ->
                resolveNewline cont0 spt expB ms
            | otherwise ->
                resolveToken cont0 spt expB ms
        Layout.ExpectBrace ->
            withL cont0 True ms

errorRecover :: Runner a
errorRecover = undefined

resolveToken :: RunnerCont a -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner a
resolveToken cont0 spt expB ms = do
    case Spanned.unSpanned spt of
        Token.SpBraceOpen | expB ->
            runParserL cont0 spt \cont1 ->
                withL cont1 False do Layout.ExplicitBrace:ms
        Token.SpDBraceOpen | expB ->
            runParserL cont0 spt \cont1 -> do
                m <- calcLayoutPos
                withL cont1 False do Layout.ExplicitDBrace m:ms
        _ | expB -> do
            let vbOp = Spanned.spannedFromLoc
                    do Spanned.beginLoc do Spanned.getSpan spt
                    do Token.SpVBraceOpen
            runParserL cont0 vbOp \cont1 -> do
                m <- calcLayoutPos
                resolveToken cont1 spt False do Layout.VirtualBrace m:ms
        t | Layout.isOpen t ->
            runParserL cont0 spt \cont1 -> do
                withL cont1 False do Layout.NoLayout:ms
        t | Layout.isClose t ->
            tryClose cont0 spt ms
        Token.LitInterpStringContinue{} ->
            tryClose cont0 spt ms
        _ ->
            runParserL cont0 spt \cont1 -> do
                withL cont1 False ms

resolveNewline :: RunnerCont a -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner a
resolveNewline cont0 spt expB ms0 = do
    let bl = Spanned.beginLoc do Spanned.getSpan spt
        c = Spanned.locCol bl
    case ms0 of
        Layout.ExplicitDBrace m:ms1
            | c < m -> case Spanned.unSpanned spt of
                Token.SpDBraceClose ->
                    resolveEmptyBrace cont0 expB bl \cont1 ->
                        runParserL cont1 spt \cont2 ->
                            withL cont2 False ms1
                _ -> error "parse error"
            | c == m ->
                resolveEmptyBrace cont0 bl \cont1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL cont1 vsemi \cont2 ->
                        resolveToken cont2 spt False ms0
            | otherwise ->
                resolveToken cont2 spt False ms0
        Layout.VirtualBrace m:ms1
            | c < m ->
                resolveEmptyBrace cont0 expB bl \cont1 -> do
                    let vbClose = Spanned.spannedFromLoc bl
                            Token.SpVBraceClose
                    runParserL cont1 vbClose \cont2 ->
                        resolveNewline cont2 spt False ms1
            | c == m ->
                resolveEmptyBrace cont0 bl \cont1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL cont1 vsemi \cont2 ->
                        resolveToken cont2 spt False ms0
            | otherwise ->
                resolveToken cont0 spt expB ms0
        _ ->
            resolveToken cont0 spt expB ms0

runParserL :: RunnerCont a -> Spanned.T Token.T
    -> (forall a. RunnerCont a -> Runner a) -> Runner a
runParserL spt cont lcont = do
    runnerModify' \ctx -> ctx
        {
            withLCont = lcont
        }
    cont spt

consumeToken :: Runner (Maybe Layout.TokenWithL)
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
        Just (Layout.Token spt) -> do
            runnerPut do
                ctx1
                    {
                        lastSpan = Spanned.getSpan spt
                    }
        _ ->
            pure ()
    pure mt

restoreToken :: TokenWithL -> Runner ()
restoreToken t = runnerModify' \ctx -> ctx
    {
        tokenStack = t:tokenStack ctx
    }

runnerGet :: Runner RunnerContext
runnerGet = Runner do Conduit.lift do get

runnerPut :: RunnerContext -> Runner ()
runnerPut x = Runner do Conduit.lift do put x

runnerModify' :: (RunnerContext -> RunnerContext) -> Runner ()
runnerModify' = Runner do Conduit.lift do modify'
