module Language.Quell.Parsing.Spanned (
    T,
    Spanned (..),
    Span (..),
    BytesSpan (..),
    Loc (..),
) where

import           Language.Quell.Prelude


type T = Spanned

data Spanned a = Spanned
    {
        getSpan :: Span,
        unSpanned :: a
    }
    deriving (Eq, Show, Functor)

data Span = Span
    {
        beginLoc :: Loc,
        endLoc   :: Loc
    }
    deriving (Eq, Show)

data BytesSpan = BytesSpan
    {
        bytesIndex :: Int,
        bytesLength :: Int
    }
    deriving (Eq, Show)

data Loc = Loc
    {
        locLine        :: Int,
        locCol         :: Int,
        locBytesPos    :: Int
    }
    deriving (Eq, Show)
