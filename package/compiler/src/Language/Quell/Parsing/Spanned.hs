module Language.Quell.Parsing.Spanned (
  Spanned (..),
  Span (..),
  Loc (..),
) where

import Language.Quell.Prelude


data Spanned a = Spanned
  {
    content :: a,
    contentSpan :: Span
  }
  deriving (Eq, Show, Functor)

data Span = Span {
  beginLoc :: Loc,
  endLoc :: Loc
} deriving (Eq, Show)

data Loc = Loc {
  locLine :: Int,
  locCol :: Int,
  locAbsPosition :: Int
} deriving (Eq, Show)
