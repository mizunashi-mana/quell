module Language.Quell.Parsing.Parser.Layout (
    T,
    Layout (..),

    TokenWithL (..),
    preParse,

    isLayoutKeyword,
    isLayoutKeywordLam,

    isOpen,
    isClose,
) where

import Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.Spanned as Spanned
import qualified Language.Quell.Parsing.Parser.Error as Error


type T = Layout

data Layout
    = NoLayout
    | ExplicitBrace
    | ExplicitDBrace Int
    | VirtualBrace Int
    deriving (Eq, Show)

data TokenWithL
    = Token Bool (Spanned.T Token.T)
    | ExpectBrace
    deriving (Eq, Show)

type WithLConduit = Conduit.ConduitT (Spanned.T Token.T) TokenWithL

preParse :: Monad m => WithLConduit m ()
preParse = go 0 isLayoutKeyword where
    go pl isL = resolveNewline pl \isN spt l -> case Spanned.unSpanned spt of
        Tok.SymLambda -> do
            Conduit.yield do Token isN spt
            go l isLayoutKeywordLam
        t | isL t -> do
            Conduit.yield do Token isN spt
            Conduit.yield ExpectBrace
            go l isLayoutKeyword
        _ -> do
            Conduit.yield do Token isN spt
            go l isLayoutKeyword

resolveNewline :: Monad m
    => Int -> (Spanned.T Token.T -> Int -> WithLConduit m ())
    -> WithLConduit m ()
resolveNewline pl cont = Conduit.await >>= \case
    Nothing ->
        pure ()
    Just spt -> do
        let sp = Spanned.getSpan spt
            l1 = Spanned.locLine do Spanned.beginLoc sp
            c1 = Spanned.locCol do Spanned.beginLoc sp
            l2 = Spanned.locLine do Spanned.endLoc sp
        if
            | pl < l1 -> do
                cont True spt l2
            | otherwise ->
                cont False spt l2

isLayoutKeyword :: Token.T -> Bool
isLayoutKeyword = \case
    Token.KwLet     -> True
    Token.KwLetrec  -> True
    Token.KwOf      -> True
    Token.KwWhen    -> True
    Token.KwWhere   -> True
    _               -> False

isLayoutKeywordLam :: Token.T -> Bool
isLayoutKeywordLam = \case
    Token.KwCase    -> True
    t               -> isLayoutKeyword t

isOpen :: Token.T -> Bool
isOpen = \case
    Token.SpParenOpen               -> True
    Token.SpBrackOpen               -> True
    Token.SpBraceOpen               -> True
    Token.SpDBraceOpen              -> True
    Token.LitInterpStringStart{}    -> True
    _                               -> False

isClose :: Token.T -> Bool
isClose = \case
    Token.SpParenClose              -> True
    Token.SpBrackClose              -> True
    Token.SpBraceClose              -> True
    Token.SpDBraceClose             -> True
    Token.LitInterpStringEnd{}      -> True
    _                               -> False
