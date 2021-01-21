module Language.Quell.Parsing.Lexer.Error (
    T,
    Error (..),
    CommentBlockKind (..),
    LayoutBlockKind (..),
) where

import Language.Quell.Prelude


type T = Error

data Error
    = UnexpectedCodeUnits -- FIXME: split more detail
    | UnconcludedBitIntegerLiteral
    | UnconcludedOctitIntegerLiteral
    | UnconcludedHexitIntegerLiteral
    | UnconcludedIntegerLiteral
    | UnclosedCommentBlock CommentBlockKind
    | UnclosedLayoutBlock LayoutBlockKind
    deriving (Eq, Show)

data CommentBlockKind
    = CommentBlockMultiline
    | CommentBlockDoc
    | CommentBlockPragma
    deriving (Eq, Ord, Show, Enum, Bounded)

data LayoutBlockKind
    = LayoutBlockBrace
    | LayoutBlockDBrace
    | LayoutBlockVBrace
    | LayoutBlockVDBrace
    deriving (Eq, Ord, Show, Enum, Bounded)
