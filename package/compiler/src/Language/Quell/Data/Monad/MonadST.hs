module Language.Quell.Data.Monad.MonadST (
    T,
    MonadST (..),
) where

import           Language.Quell.Prelude


type T = MonadST

class (Monad m, Marker m ~ s) => MonadST s m where
    type Marker m :: Type
    liftST :: ST s a -> m a

instance MonadST s (ST s) where
    type Marker (ST s) = s
    liftST mx = mx

instance MonadST RealWorld IO where
    type Marker IO = RealWorld
    liftST mx = stToIO mx

instance MonadST s m => MonadST s (StateT a m) where
    type Marker (StateT a m) = Marker m
    liftST mx = StateT \s0 -> do
        x <- liftST mx
        pure (x, s0)
