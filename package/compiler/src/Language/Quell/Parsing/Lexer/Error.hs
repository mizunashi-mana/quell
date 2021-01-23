module Language.Quell.Parsing.Lexer.Error (
    T,
    Error (..),
    CommentBlockKind (..),
    LayoutBlockKind (..),
) where

import           Language.Quell.Prelude


type T = Error

data Error
    = UnexpectedCodeUnits -- FIXME: split more detail
    | UnclosedByteStringLiteral
    | NonAsciiCharInByteStringLiteral
    | NonGraphicInByteStringLiteral
    | UniEscapeInByteStringLiteral
    | UnclosedByteCharLiteral
    | NoContentByteCharLiteral
    | TooManyContentsInByteCharLiteral
    | NonAsciiCharInByteCharLiteral
    | NonGraphicInByteCharLiteral
    | UniEscapeInByteCharLiteral
    | GapInByteCharLiteral
    | UnclosedStringLiteral
    | NonGraphicInStringLiteral
    | UnclosedCharLiteral
    | NoContentCharLiteral
    | TooManyContentsInCharLiteral
    | NonGraphicInCharLiteral
    | GapInCharLiteral
    | UnclosedInterpStringLiteral
    | NonGraphicInInterpStringLiteral
    | InvalidInterpOpenInInterpStringLiteral
    | NonGraphicInLineComment
    | UnclosedMultilineComment
    | NonGraphicInMultilineComment
    | UnclosedDocComment
    | NonGraphicInDocComment
    | UnclosedPragmaComment
    | NonGraphicInPragmaComment
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
