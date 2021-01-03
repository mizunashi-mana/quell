module Language.Quell.Data.Spanned (
  Spanned (..),
  Span (..),
  Loc (..),
) where


data Spanned a = Spanned Span a
  deriving (Eq, Show, Functor)

data Span = Span {
  beginLoc :: Loc,
  endLoc :: Loc
} deriving (Eq, Show)

data Loc = Loc {
  locLine :: Int,
  locCol :: Int
} deriving (Eq, Show)
