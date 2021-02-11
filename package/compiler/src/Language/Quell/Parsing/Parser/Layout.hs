module Language.Quell.Parsing.Parser.Layout (
    T,
    Layout (..),

    isLayoutKeyword,
    isLayoutKeywordLam,

    isOpen,
    isClose,
) where

import Language.Quell.Prelude

import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.Error as Error


type T = Layout

data Layout
    = NoLayout
    | ExplicitBrace
    | ExplicitDBrace Int
    | VirtualBrace Int
    deriving (Eq, Show)

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
