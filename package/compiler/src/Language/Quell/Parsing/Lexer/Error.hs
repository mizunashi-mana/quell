module Language.Quell.Parsing.Lexer.Error (
    T,
    Error (..),
) where

import           Language.Quell.Prelude


type T = Error

data Error
    = UnexpectedCodeUnits
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
