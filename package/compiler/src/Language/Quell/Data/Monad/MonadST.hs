module Language.Quell.Data.Monad.MonadST (
    T,
    MonadST (..),
) where

import Language.Quell.Prelude


type T = MonadST

class Monad m => MonadST m where
    type Marker m :: Type
    liftST :: ST (Marker m) a -> m a

instance MonadST (ST s) where
    type Marker (ST s) = s
    liftST mx = mx

instance MonadST IO where
    type Marker IO = RealWorld
    liftST mx = stToIO mx

instance MonadST m => MonadST (StateT s m) where
    type Marker (StateT s m) = Marker m
    liftST mx = StateT \s0 -> do
        x <- liftST mx
        pure (x, s0)
