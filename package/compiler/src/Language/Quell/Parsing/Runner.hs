module Language.Quell.Parsing.Runner (
  Runner (..),
  Context (..),
  Result (..),

  LayoutContext (..),
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Spanned as Spanned


newtype Runner a = Runner {
  runRunner :: StateT Context Result a
} deriving (Functor, Applicative, Monad) via StateT Context Result

data Context = Context {
  currentLoc  :: Spanned.Loc,
  layoutStack :: [LayoutContext]
} deriving (Eq, Show)

data Result a
  = ParseOk a
  | ParseFailed
  deriving (Eq, Show, Functor)

instance Applicative Result where
  pure x = ParseOk x

  mf <*> mx = case mf of
    ParseFailed -> ParseFailed
    ParseOk f   -> case mx of
      ParseFailed -> ParseFailed
      ParseOk x   -> ParseOk do f x

instance Monad Result where
  mx >>= f = case mx of
    ParseFailed -> ParseFailed
    ParseOk x   -> f x

data LayoutContext
  = NoLayout
  | Layout Int
  deriving (Eq, Show)
