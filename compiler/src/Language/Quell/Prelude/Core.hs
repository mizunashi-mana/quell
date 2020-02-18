module Language.Quell.Prelude.Core (
  module Prelude,
  module Control.Applicative,
  module Control.Monad,
  module Control.Monad.IO.Class,
  module Data.ByteString,
  module Data.Foldable,
  module Data.Functor,
  module Data.Kind,
  module Data.MonoTraversable,
  module Data.Text,

  MonoTraversableM (..),
  oforM,
  ocount,
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                   (ByteString)
import           Data.Foldable                     hiding (foldl, foldr, foldr')
import           Data.Functor
import           Data.Kind                         (Type)
import           Data.MonoTraversable              hiding (omapM, oforM)
import           Data.Text                         (Text)
import           Prelude                           hiding (foldl, foldr, head, tail, String)


class MonoTraversable mono => MonoTraversableM mono where
  omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

oforM :: forall m mono. Monad m => MonoTraversableM mono =>
  mono -> (Element mono -> m (Element mono)) -> m mono
oforM xs f = omapM f xs

ocount :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Int
ocount f ys = ofoldl' (\acc y -> acc + if f y then 1 else 0) 0 ys
