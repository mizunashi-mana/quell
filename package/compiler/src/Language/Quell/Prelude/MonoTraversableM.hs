module Language.Quell.Prelude.MonoTraversableM (
  MonoTraversableM (..),
  oforM,
  ocount,
) where

import           Language.Quell.Prelude.Core


class MonoTraversable mono => MonoTraversableM mono where
  omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

oforM :: forall m mono. Monad m => MonoTraversableM mono =>
  mono -> (Element mono -> m (Element mono)) -> m mono
oforM xs f = omapM f xs

ocount :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Int
ocount f ys = ofoldl' (\acc y -> if f y then acc + 1 else acc) 0 ys
