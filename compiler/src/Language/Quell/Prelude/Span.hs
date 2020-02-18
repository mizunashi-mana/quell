module Language.Quell.Prelude.Span where

import Language.Quell.Prelude.Core


data Spanned a = Spanned () a
  deriving Show

deriving instance Eq a => Eq (Spanned a)
