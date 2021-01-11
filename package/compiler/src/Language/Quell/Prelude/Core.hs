module Language.Quell.Prelude.Core (
  module Prelude,

  module Control.Applicative,
  module Control.Monad.IO.Class,
  module Control.Monad.Trans.Reader,
  module Control.Monad.Trans.State.Strict,
  module Data.ByteString,
  module Data.Coerce,
  module Data.Constraint,
  module Data.Function,
  module Data.Functor,
  module Data.Functor.Identity,
  module Data.Functor.Compose,
  module Data.Ix,
  module Data.Kind,
  module Data.List.NonEmpty,
  module Data.MonoTraversable,
  module Data.Ord,
  module Data.Proxy,
  module Data.Sequences,
  module Data.Text,
  module Data.Typeable,
  module Data.Word,
  module GHC.Prim,
  module GHC.Stack,
  module Prettyprinter,
) where

import           Prelude                          hiding (String, foldl, foldr,
                                                   head, pi, tail, ($), takeWhile, take, splitAt, span, reverse, replicate, dropWhile, drop, break, words, unwords, unlines, lines, filter)

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict hiding (modify, liftCallCC, liftCatch)
import           Control.Monad.Trans.Reader       hiding (liftCallCC, liftCatch)
import           Data.ByteString                  (ByteString)
import           Data.Coerce
import           Data.Function                    hiding (($))
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Ix                          (Ix)
import           Data.Kind                        (Type)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.MonoTraversable              hiding (omapM, oforM)
import           Data.Ord                         (Down (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Sequences
import           Data.Text                         (Text)
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word, Word8)
import           GHC.Prim
import           Prettyprinter                     (Pretty (..), Doc)
import Data.Constraint (Dict (..), withDict)
import GHC.Stack (CallStack, HasCallStack, callStack)
