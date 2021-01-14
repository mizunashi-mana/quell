module Language.Quell.Type.ErrorCode (
  T,
  ErrorCode (..),
  ErrorLevel (..),
  toInt,
  toLevel,
) where

import           Language.Quell.Prelude


type T = ErrorCode

data ErrorCode
  = Unknown

  -- lexer error
  | LexBreakEncoding
  | LexUnclosedCommentBlock
  deriving (Eq, Ord, Enum, Bounded, Show)

data ErrorLevel
  = Bug
  | CriticalError
  | RecoverableError
  | Warning
  | Suggestion
  | Note
  | Message
  deriving (Eq, Ord, Enum, Bounded, Show)

toInt :: ErrorCode -> Int
toInt = \case
  Unknown                 -> 1
  LexBreakEncoding        -> 1000
  LexUnclosedCommentBlock -> 1001

toLevel :: ErrorCode -> ErrorLevel
toLevel = \case
  Unknown                 -> Bug
  LexBreakEncoding        -> RecoverableError
  LexUnclosedCommentBlock -> RecoverableError
