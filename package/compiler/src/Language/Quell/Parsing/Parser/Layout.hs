module Language.Quell.Parser.Layout (
    T,
    Layout (..),

    LayoutStack (..),
    empty,
) where

import Language.Quell.Prelude

import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.Error as Error


type T = Layout

data Layout
    = NoLayout
    | DBraceLayout Int
    | VBraceLayout Int
    deriving (Eq, Show)

newtype LayoutStack = LayoutStack [Layout]
    deriving (Eq, Show)

data LParseResult
    = LParseCont Token.T
    | LParseOk
    | LParseError Error.T
    deriving (Eq, Show)

empty :: LayoutStack
empty = LayoutStack []

pushLayout :: Layout -> LayoutStack -> LayoutStack
pushLayout l (LayoutStack ls) = LayoutStack do l:ls

popLayout :: LayoutStack -> LayoutStack
popLayout = \case
    LayoutStack (_:ls)  -> LayoutStack ls
    LayoutStack []      -> LayoutStack []
