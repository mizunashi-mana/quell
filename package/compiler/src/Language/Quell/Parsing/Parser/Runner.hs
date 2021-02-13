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


type RunnerContinue a = Maybe (Spanned.T Token.T) -> Runner a

data RunnerContext = RunnerContext
    {
        withLCont :: Maybe (RunnerContinue a -> Runner a),
        lastSpan :: Spanned.Span,
        tokenStack :: [Layout.TokenWithL],
        layoutStack :: [Layout.T],
        errors :: Bag.T Error.T
    }

initialContext :: RunnerContext
initialContext = RunnerContext
    {
        withLCont = Nothing,
        lastSpan = Spanned.Span
            {
                Spanned.beginLoc = lastLoc,
                Spanned.endLoc = lastLoc
            },
        tokenStack = [],
        layoutStack = [],
        errors = mempty
    }
    where
        lastLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            }

lexer :: (Spanned.T Token.T -> Runner a) -> Runner a
lexer cont0 = do
    ctx0 <- runnerGet
    runnerPut do
        ctx0
            {
                withLCont = Nothing
            }
    case withLCont ctx0 of
        Nothing ->
            go
        Just lcont -> do
            lcont cont1
    where
        cont1 = \case
            Just spt ->
                cont0 spt
            Nothing ->
                go

withL :: (Spanned.T Token.T -> Runner a) -> Runner a
withL cont = consumeToken >>= \case
    Nothing ->
        tryEnd cont
    Just twl -> case twl of
        Layout.Token spt -> case Spanned.unSpanned spt of
            t | Layout.isOpen t -> do
                runnerModify' \ctx -> ctx
                    {
                        layoutStack = Layout.NoLayout:layoutStack ctx
                    }
                cont spt
            t | Layout.isClose t ->
                tryClose cont spt
            Token.LitInterpStringContinue{} ->
                tryClose cont spt
            _ ->
                cont spt
        Layout.ExpectBrace ->
            resolveBraceOpen cont
        Layout.Newline n ->
            resolveNewline n cont

errorRecover :: Runner a
errorRecover = undefined

resolveBraceOpen :: (Spanned.T Token.T -> Runner a) -> Runner a
resolveBraceOpen cont = consumeToken >>= \case
    Nothing -> do
        ctx0 <- runnerGet
        openVBrace do Spanned.endLoc do lastSpan ctx0
    Just twl -> case twl of
        Layout.Token spt -> case Spanned.unSpanned spt of
            Token.SpBraceOpen -> do
                runnerModify' \ctx ->
                    ctx
                        {
                            layoutStack = Layout.ExplicitBrace:layoutStack ctx
                        }
                cont spt
            Token.SpDBraceOpen -> do
                m <- calcLayoutPos
                runnerModify' \ctx ->
                    ctx
                        {
                            layoutStack = Layout.ExplicitDBrace m:layoutStack ctx
                        }
                cont spt
            _ -> do
                restoreToken twl
                openVBrace do Spanned.endLoc do Spanned.getSpan spt
        _ -> do
            ctx0 <- runnerGet
            openVBrace do Spanned.endLoc do lastSpan ctx0
    where
        openVBrace l = do
            m <- calcLayoutPos
            runnerPut do
                ctx0
                    {
                        layoutStack = Layout.VirtualBrace m:layoutStack ctx
                    }
            cont do Spanned.spannedFromLoc l Token.SpVBraceOpen

resolveNewline :: Int -> (Spanned.T Token.T -> Runner a) -> Runner a
resolveNewline n cont = do
    ctx0 <- runnerGet
    case layoutStack ctx0 of
        Layout.ExplicitDBrace m:_
            | n < m ->

        [] ->


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
